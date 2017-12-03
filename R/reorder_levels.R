#' This function changes the order of levels in the data.
#' 
#' @param dafr The data to be changed
#' @param column the factor column where the order of 
#' levels should be changed.
#' @param levels.new A vector of all levels in the column 
#' specified by column in the order they should be 
#' ordered.
#' 
#' @return A data.frame with the levels of one column 
#' reordered
#' 
#' @author Christoph Knapp
reorder.levels = function(dafr,column,levels.new){
  dafr[,column] = factor(dafr[,column],levels=levels.new)
  dafr
}