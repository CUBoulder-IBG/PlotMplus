#' Plots the loading matrix
#' @author Subrata Paul
#' @description Given the mplus output the function extract the loading matrix
#' and make a box plot.
#' @param model_output Output from mplus run
#' @param load.thres Threshold of loading to avoid plotting. Default is to plot all laoding.
#' @param sort Logical. If true the variables will be sorted using `fa.sort` function.
#' @import ggplot2
#' @import grid
#' @import gridExtra
#' @import tidyverse
#' @import MplusAutomation
#' @export


plot_loadings<-function(model_output, load.thres = NA, sort = T){
  main_model = readModels(model_output)
  params = main_model$parameters$stdyx.standardized
  by_params = params[grep('BY', params$paramHeader),]
  nfactors = length(unique(by_params$paramHeader))
  nparams = length(unique(by_params$param))
  loadings = matrix(by_params$est, ncol = nfactors)
  row.names(loadings) = by_params$param[1:nparams]
  loadings = psych::fa.sort(loadings)

  plot_dat = by_params%>%group_by(paramHeader)%>%arrange(paramHeader, match(param, row.names(loadings)))
  plot_dat$pval[plot_dat$pval==0]<-2e-16
  plot_dat$est_only_significant = plot_dat$est*(plot_dat$pval<0.05)
  plot_dat$se_only_significant = plot_dat$se*(plot_dat$pval<0.05)
  plot_dat$est_only_significant_NA = plot_dat$est_only_significant
  plot_dat$est_only_significant_NA[plot_dat$est_only_significant_NA == 0]<-NA
  if(!is.na(load.thres)){
    plot_dat$est_only_significant_NA[abs(plot_dat$est_only_significant_NA) < load.thres]<-NA
  }
  if(sort){
    plot_dat$param = factor(plot_dat$param, levels = row.names(loadings))
  }

  plot_dat$paramHeader = gsub('.BY','',plot_dat$paramHeader)

  title = paste0('CFI = ', main_model$summaries$CFI, ', RMSEA = ', main_model$summaries$RMSEA_Estimate)
  if(!is.na(load.thres)){
    title = paste0(title, ' ; Showing loading >', load.thres)
  }
  ggplot(plot_dat, aes(param, est_only_significant_NA, fill = -1*log(plot_dat$pval)))+
    facet_wrap(~paramHeader, nrow = 1)+
    geom_bar(stat = 'identity', position = position_dodge())+
    geom_errorbar(aes(ymin = est_only_significant_NA - se_only_significant, ymax = est_only_significant_NA+se_only_significant))+
    geom_text(aes(x = param, y = 0, label = round(est_only_significant_NA,2)), hjust = 'left')+
    coord_flip()+
    ggtitle(title)+
    labs(fill = '-log(p-value)', y = 'Loadings', x = 'Conditions')
}
