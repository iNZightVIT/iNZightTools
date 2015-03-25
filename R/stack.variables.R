#' Stacks the selected columns onto the data.
#'
#' Multiplies the data.set by adding rows to the data for 
#' every selected column. The selected columns are stacked
#' onto each other and added as an additional column 
#' 
#' @param columns The columns to stack.
#' @param dafr a dataframe the stacking is performed on.
#' 
#' @author Christoph Knapp
stack.variables.perform = function(columns,dafr){
  stack = unlist(lapply(1:length(columns),function(index,d,c){
    d[,which(colnames(d)%in%c[index])]
  },dafr,columns))
  colstack = unlist(lapply(1:length(columns),function(index,d,c){
    rep(c[index],nrow(d))
  },dafr,columns))
  cbind(dafr,stack.columns=colstack,stack.variables=stack)
}