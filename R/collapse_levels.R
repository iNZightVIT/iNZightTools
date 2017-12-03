#' Collapses selected levels in factor vector
#' 
#' @param column the vector where levels should be 
#' collapsed into one.
#' @param to.collapse Vector of levels to collapse.
#' 
#' @note Levels in to.collapse which are not in column 
#' will be ignored.
#' 
#' @author Christoph Knapp
get.collapsed.column = function(column,to.collapse){
  column = as.character(column)
  new.level = paste(to.collapse,collapse=".")
  indices = which(column%in%to.collapse)
  if(length(indices)>0){
    column[indices] = new.level
  }
  as.factor(column)
}