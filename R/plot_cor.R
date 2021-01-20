#' Plot correlation matrix
#' @author Subrata Paul
#' @param model_output The output from mplus model
#' @export

plot_cor<-function(model_output){
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
  rownames(corr) = by_params$param[1:nparams]
  names(corr) = by_params$param[1:nparams]
  corr = corr[rownames(loadings),rownames(loadings)]
  corrplot::corrplot(as.matrix(corr))
  #ggcorrplot::ggcorrplot(as.matrix(corr))
}
