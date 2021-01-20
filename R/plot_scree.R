#'Plot scree plot
#'
#' @param model_output The output from mplus model
#' @export

plot_scree<-function(model_output){
  main_model = readModels(model_output)
  corr = main_model$sampstat$correlations.vardiag
  corr[upper.tri(corr)]=0
  if(nrow(corr)>ncol(corr)){
    corr = cbind(corr, rep(0,nrow(corr)))
  }

  corr = corr + t(corr)
  diag(corr) = 1
  eigen_values = eigen(corr)$values
  ggplot(data = data.frame(Index = 1:length(eigen_values), Eigenvalues = eigen_values),
         aes(x = Index, y = Eigenvalues))+
    geom_point()+
    geom_abline(intercept = 1, slope = 0, color = 'red')
}
