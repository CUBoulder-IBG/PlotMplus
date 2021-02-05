#' Makes a table with associations
#' @param model_output The output from mplus model
#' @param model The model extracted using `MplusAutomation::readModel`
#'
#' @description `plot_assoc` takes the parameter estimate from the model output
#' that are associated with 'ON' operator.
#' @return A table that contains the parameter estimate, standard error and p-value
#' for each `ON` relationship.
#' @export

plot_assoc<-function(model_output=NA, model = NA, indep.var){
  ifelse(is.na(model), main_model <- MplusAutomation::readModels(model_output),
         main_model<-model)
  params = main_model$parameters$stdyx.standardized
  on_params = params[grep('ON', params$paramHeader),]
  on_params = params[params$param==indep.var,]
  on_params$paramHeader <- gsub('.ON','',on_params$paramHeader)
  on_params$est_and_se = paste0(on_params$est, ' (SE= ', on_params$se,')\n p-value =',on_params$pval)
  names(on_params)[1] = 'Factors'
  assoc_table = on_params%>%select(Factors, param, est_and_se)%>%spread(param, est_and_se)
  p_table = on_params%>%select(Factors, param, pval)%>%spread(param, pval)
  highlight_cell = which(p_table<0.05, arr.ind = T)

  grid.table(assoc_table)


  g <- tableGrob(assoc_table, rows = NULL)
  find_cell <- function(table, row, col, name="core-fg"){
    l <- table$layout
    which(l$t==row & l$l==col & l$name==name)
  }

  for(i in 1:nrow(highlight_cell)){
    ind = find_cell(g, highlight_cell[i,1]+1, highlight_cell[i,2], 'core-bg')
    g$grobs[ind][[1]][['gp']]<-gpar(fill = 'darkolivegreen1', col = 'darkolivegreen4', lwd = 5)
  }

  #grid.newpage()
  #grid.draw(g)
  return(g)
}
