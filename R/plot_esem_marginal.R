#' Plots parameters in an ESEM model
#' @author Subrata Paul
#' @param model_output The output from MPLUS
#' @param covariates A character vector of covariates. If the dependent variables were
#' regressed on some variables they are the covariates.
#' @param indep.var The variables that were regressed on the factors
#' @param load.thres Threshold of loading to include in the plot. Default is 0.2
#' @param sort Logical. If true the loadings will be sorted using `fa.sort` function
#' @param marginal Dataframe. Contains marginal association of the independent variable with the dependents.
#' The dataframe contains columns with names "Condition", "Estimate", "SE", "Tstat", "Pvalue", "R2" , "Tstat_times_R2"
#' @param marginal_show Either `Estimate` or `Tstat_times_R2`. The selected statistic will be showed.
#' @import MplusAutomation
#' @import ggplot2
#' @import tidyverse
#' @examples{
#' mplus_out = system.file("extdata", "sample_esem.out", package = "PlotMplus")
#' plot_esem(model_output = mplus_out, covariates = c('AGE','SEX','BMI'),indep.var = 'CREACT',sort = T)
#' }
#' @export
plot_esem_marginal<-function(model_output, marginal=NA, marginal_show, covariates = NA, indep.var=NA,load.thres = 0.2, sort = F,load.text.size=5){
  main_model = readModels(model_output)
  params = main_model$parameters$stdyx.standardized
  by_params = params[grep('BY', params$paramHeader),]
  by_params$paramHeader = gsub('.BY','',by_params$paramHeader)
  nfactors = length(unique(by_params$paramHeader))
  nparams = length(unique(by_params$param))
  loadings = matrix(by_params$est, ncol = nfactors)
  row.names(loadings) = by_params$param[1:nparams]
  loadings = psych::fa.sort(loadings)
  indep.var = c(indep.var)
  sorted_contidion <- c(indep.var,row.names(loadings))
  plot_dat = by_params%>%group_by(paramHeader)%>%arrange(paramHeader, match(param, sorted_contidion))



  if(!is.na(covariates)){
    on_params = params[grep('ON', params$paramHeader),]
    on_params = on_params[on_params$param%in%covariates, ]
    on_params$paramHeader = gsub('.ON','',on_params$paramHeader)
    flag = on_params$paramHeader
    on_params$paramHeader = on_params$param
    on_params$param = flag
    on_params$param[on_params$param=='SAI'] <- 'SPONAI'
    plot_dat = rbind(plot_dat, on_params%>%group_by(paramHeader)%>%arrange(paramHeader, match(param,sorted_contidion)))
  }

  if(!is.na(indep.var)){
    on_params = params[grep('ON', params$paramHeader),]
    on_params = on_params[on_params$param%in%indep.var,]
    on_params$paramHeader = gsub('.ON','',on_params$paramHeader)
    flag = on_params$paramHeader
    plot_dat = rbind(plot_dat, on_params%>%group_by(paramHeader)%>%arrange(paramHeader, match(param,c(indep.var,sorted_contidion))))
  }

  plot_dat$pval[plot_dat$pval<2e-16]<-2e-16
  plot_dat$est[plot_dat$pval>0.05]<-NA
  plot_dat$se[plot_dat$pval>0.05]<-NA



  if(!is.na(load.thres)){
    plot_dat$est[!(plot_dat$param%in%indep.var) & plot_dat$paramHeader!=indep.var & (abs(plot_dat$est) < load.thres)]<-NA
  }

  if(sort){
    plot_dat$param = factor(plot_dat$param, levels = sorted_contidion)
  }else{
    plot_dat$param = factor(plot_dat$param, levels = c(indep.var,unique(by_params$param)))
  }




  title = paste0('CFI = ', main_model$summaries$CFI, ', RMSEA = ', main_model$summaries$RMSEA_Estimate)
  if(!is.na(load.thres)){
    title = paste0(title, '; \n Showing loading >', load.thres)
  }

  if(!is.na(indep.var)){
    title = paste0(title, ' ; Association with ', indep.var)
  }


  plot_dat$paramHeader = factor(plot_dat$paramHeader, levels = c(unique(by_params$paramHeader),covariates,indep.var))

  p = ggplot(plot_dat[plot_dat$paramHeader!=indep.var,], aes(param, est, fill = -1*log(pval)))+
    facet_wrap(~paramHeader, nrow = 1)+
    geom_bar(stat = 'identity', position = position_dodge())+coord_flip()+
    theme(axis.text.x = element_text(angle = 90))+ylim(-0.5,1.1)
  if(!is.na(indep.var)){
    p = p +
      geom_rect(xmin = as.numeric(plot_dat$param[[nrow(plot_dat)]]) - 0.5,
                xmax = as.numeric(plot_dat$param[[nrow(plot_dat)]]) + 0.5,
                ymin = -Inf, ymax = Inf,fill = 'red', alpha=0.01)
  }

  p=p+geom_errorbar(aes(ymin = est - se, ymax = est+se), color = 'blue', width = 0.5)+
    geom_text(aes(x = param, y = 0, label = round(est,2)), color = "#D55E00", size = load.text.size, hjust = 'left',nudge_y = -0.5)+
    ggtitle(title)+
    labs(fill = '-log(p-value)', y = 'Loadings', x = 'Conditions')
  p = p+theme(legend.position = 'none')


  if(!is.na(marginal)){
    marg<-read.table(marginal,header = T)
    marg<-marg%>%select(Condition, Estimate, SE, Tstat, Pvalue)%>%
      mutate(Condition = toupper(Condition))
    row.names(marg)<-marg$Condition
    marg = marg[row.names(loadings),]

    if(marginal_show=='OR'){
      marg = marg%>%mutate(Estimate = exp(Estimate))
    }
    marg = rbind(data.frame(Condition = indep.var, Estimate = NA, SE = NA, Tstat = NA, Pvalue = NA), marg)
    marg = marg%>%mutate(paramHeader = rep(indep.var, nrow(marg)))



    if(sort){
      marg$Condition = factor(marg$Condition, levels = sorted_contidion)
    }else{
      marg$Condition = factor(marg$Condition, levels = c(indep.var,unique(by_params$param)))
    }


    marg$Pvalue[marg$Pvalue<2e-16]<-2e-16
    marg$Estimate[marg$Pvalue>0.05]<-NA
    marg$SE[marg$Pvalue>0.05]<-NA


    scaleFUN <- function(x) sprintf("%.2f", x)

    p_mar<-ggplot(data = marg,
                  aes(Condition, Estimate, fill = -1*log(Pvalue)))+
      geom_bar(stat = 'identity') + coord_flip()+
      theme(axis.text.y = element_blank(),axis.text.x = element_text(angle = 40),
            axis.ticks.y = element_blank(), axis.title.y = element_blank(),
            plot.margin = margin (5.5, 5.5, 3.5, -1))+
      ggtitle(' \n ')+facet_wrap(~paramHeader)+labs(y = marginal_show)+
      geom_errorbar(aes(ymin = Estimate - SE, ymax = Estimate + SE), color = 'blue', width = 0.5)+
      scale_y_continuous(labels=scaleFUN)

    p_mar = p_mar + geom_rect(xmin = as.numeric(plot_dat$param[[nrow(plot_dat)]]) - 0.5,
                              xmax = as.numeric(plot_dat$param[[nrow(plot_dat)]]) + 0.5,
                              ymin = -Inf, ymax = Inf,fill = 'green', alpha=0.01)

    out<-gridExtra::grid.arrange(p,p_mar, nrow= 1, widths = c(length(unique(plot_dat$paramHeader)),1.5))
  }else{
    out<-p
  }

  return(out)
}


# plot_esem_marginal<-function(model_output, marginal, marginal_show, covariates = NA, indep.var=NA,load.thres = 0.2, sort = F,load.text.size=5){
#   main_model = readModels(model_output)
#   params = main_model$parameters$stdyx.standardized
#   by_params = params[grep('BY', params$paramHeader),]
#   by_params$paramHeader = gsub('.BY','',by_params$paramHeader)
#   nfactors = length(unique(by_params$paramHeader))
#   nparams = length(unique(by_params$param))
#   loadings = matrix(by_params$est, ncol = nfactors)
#   row.names(loadings) = by_params$param[1:nparams]
#   loadings = psych::fa.sort(loadings)
#   indep.var = c(indep.var)
#   sorted_contidion <- c(indep.var,row.names(loadings))
#   plot_dat = by_params%>%group_by(paramHeader)%>%arrange(paramHeader, match(param, sorted_contidion))
#   if(!is.na(indep.var)){
#     marg<-read.table(marginal,header = T)
#
#     marg<-marg%>%select(Condition, Estimate, SE, Tstat, Pvalue)%>%
#     mutate(Condition = toupper(Condition))%>%
#     rename(param = Condition, est = Estimate, se = SE, pval = Pvalue, est_se = Tstat)%>%
#     mutate(paramHeader = rep(indep.var, nrow(marg)))%>%
#     select(paramHeader, param, est, se, est_se, pval)
#     marg = marg[marg$param%in%plot_dat$param,]
#
#
#     if(marginal_show=='OR'){
#       marg = marg%>%mutate(est = exp(est), se = se)
#     }
#     plot_dat <- rbind(plot_dat, marg)
#   }
#
#
#   if(!is.na(covariates)){
#     on_params = params[grep('ON', params$paramHeader),]
#     on_params = on_params[on_params$param%in%covariates, ]
#     on_params$paramHeader = gsub('.ON','',on_params$paramHeader)
#     flag = on_params$paramHeader
#     on_params$paramHeader = on_params$param
#     on_params$param = flag
#     on_params$param[on_params$param=='SAI'] <- 'SPONAI'
#     plot_dat = rbind(plot_dat, on_params%>%group_by(paramHeader)%>%arrange(paramHeader, match(param,sorted_contidion)))
#   }
#
#   if(!is.na(indep.var)){
#     on_params = params[grep('ON', params$paramHeader),]
#     on_params = on_params[on_params$param%in%indep.var,]
#     on_params$paramHeader = gsub('.ON','',on_params$paramHeader)
#     flag = on_params$paramHeader
#     plot_dat = rbind(plot_dat, on_params%>%group_by(paramHeader)%>%arrange(paramHeader, match(param,c(indep.var,sorted_contidion))))
#   }
#
#   plot_dat$pval[plot_dat$pval<2e-16]<-2e-16
#   plot_dat$est[plot_dat$pval>0.05]<-NA
#   plot_dat$se[plot_dat$pval>0.05]<-NA
#
#
#
#   if(!is.na(load.thres)){
#     plot_dat$est[!(plot_dat$param%in%indep.var) & plot_dat$paramHeader!=indep.var & (abs(plot_dat$est) < load.thres)]<-NA
#   }
#
#   if(sort){
#     plot_dat$param = factor(plot_dat$param, levels = sorted_contidion)
#   }else{
#     plot_dat$param = factor(plot_dat$param, levels = c(indep.var,unique(by_params$param)))
#   }
#
#
#
#
#   title = paste0('CFI = ', main_model$summaries$CFI, ', RMSEA = ', main_model$summaries$RMSEA_Estimate)
#   if(!is.na(load.thres)){
#     title = paste0(title, '\n ; Showing loading >', load.thres)
#   }
#
#   if(!is.na(indep.var)){
#     title = paste0(title, ' ; Association with ', indep.var)
#   }
#
#
#   plot_dat$paramHeader = factor(plot_dat$paramHeader, levels = c(unique(by_params$paramHeader),covariates,indep.var))
#
#   p = ggplot(plot_dat, aes(param, est, fill = -1*log(pval)))+
#     facet_wrap(~paramHeader, nrow = 1)+
#     geom_bar(stat = 'identity', position = position_dodge())+coord_flip()+
#     theme(axis.text.x = element_text(angle = 90))+ylim(-0.5,1.1)
#   if(!is.na(indep.var)){
#     p = p +
#       geom_rect(xmin = as.numeric(plot_dat$param[[nrow(plot_dat)]]) - 0.5,
#                 xmax = as.numeric(plot_dat$param[[nrow(plot_dat)]]) + 0.5,
#                 ymin = -Inf, ymax = Inf,fill = 'red', alpha=0.01)
#   }
#
#   p=p+geom_errorbar(aes(ymin = est - se, ymax = est+se), color = 'blue', width = 0.5)+
#     geom_text(aes(x = param, y = 0, label = round(est,2)), color = "#D55E00", size = load.text.size, hjust = 'left',nudge_y = -0.5)+
#     ggtitle(title)+
#     labs(fill = '-log(p-value)', y = 'Loadings', x = 'Conditions')
#   return(p)
# }
