#' Plots correlation matrix of the factors
#' @param model_output Output from Mplus model
#' @return Correlation plot of factors
#' @import MplusAutomation
#' @export

plot_factor_cor<-function(model_output){
  params = model_output$parameters$stdyx.standardized
  f.cor = params[grep('WITH',params$paramHeader),]
  n.factor = 0.5 + sqrt(1+8*nrow(f.cor))/2
  corr = matrix(0, nrow = n.factor, ncol = n.factor)
  corr[upper.tri(corr, diag = F)] <- f.cor$est
  corr = t(corr)
  corr = corr + t(corr)
  colnames(corr)<-paste0('F', 1:ncol(corr))
  rownames(corr)<-paste0('F', 1:nrow(corr))
  corrplot::corrplot.mixed(corr, lower = 'number', upper = 'ellipse')
}
