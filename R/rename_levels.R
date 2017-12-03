#' Renames the levels of a factor.
#'
#' @param dafr A data.frame of the data to change.
#' @param column The column name of the column to change.
#' @param new.levels A character variable of the length of 
#' the number of factors of the column to change. This 
#' vector contains the new levels.
#'
#' @return A data.frame where the levels of the specified 
#' columns are changed.
#' 
#' @author Christoph Knapp
rename.levels = function(dafr,column,new.levels){
  temp = as.character(dafr[,column])
  for(i in 1:length(levels(dafr[,column]))){
    temp[which(dafr[,column]%in%levels(dafr[,column])[i])] = new.levels[i]
  }
  dafr[,column] = factor(temp,levels=new.levels)
  dafr
}
