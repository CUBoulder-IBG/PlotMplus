#' Plot correlation matrix
#' @author Subrata Paul
#' @param model_output The output from mplus model
#' @export

plot_cor<-function(model_output, indp_var='IND_VAR'){
  main_model = readModels(model_output)
  params = main_model$parameters$stdyx.standardized
  by_params = params[grep('BY', params$paramHeader),]
  nfactors = length(unique(by_params$paramHeader))
  nparams = length(unique(by_params$param))
  loadings = matrix(by_params$est, ncol = nfactors)
  row.names(loadings) = by_params$param[1:nparams]
  loadings = psych::fa.sort(loadings)
  cond_order = rownames(loadings)

  corr = main_model$sampstat$correlations.vardiag
  corr[upper.tri(corr)]=0
  if(nrow(corr)>ncol(corr)){
    corr = cbind(corr, rep(0,nrow(corr)))
  }

  corr = corr + t(corr)
  corr = as.data.frame(corr)
  load_order<-rownames(loadings)
  if(nrow(corr)==nparams + 1){
    row_col_names<-c(by_params$param[1:nparams], indp_var)
    load_order<-c(rownames(loadings), indp_var)
  }
  row_col_names<-by_params$param[1:nparams]
  rownames(corr) = row_col_names
  names(corr) = row_col_names

  corr = corr[load_order,load_order]
  corrplot::corrplot(as.matrix(corr))
  #ggcorrplot::ggcorrplot(as.matrix(corr))
}
