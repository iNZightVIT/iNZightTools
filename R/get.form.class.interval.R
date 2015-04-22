#' Form class intervals from a column specified by column 
#' name or column index from the a data.frame.
#' 
#' @param dafr The data frame the column can be retrieved from.
#' @param column The column name or index to produce class 
#' intervals from.
#' @param num.intervals The number of intervals the column 
#' should be separated in.
#' @param open.left.closed.right Logical variable specifying 
#' whether the output should be in the format 
#' [open left, closed right) or (closed left, open right] 
#' @param method The method used to generate the class 
#' intervals.
#' @param intervals If method="specified" this needs to be 
#' provided as a vector of numeric values. The length of the 
#' vector needs to be num.intervals-1 and the minimum and 
#' maximum needs to be within range of the minimum and maximum 
#' of the selected column.
#' @param labels Optional labels for the intervals. By default 
#' the range of the labels will be used.
#' 
#' @note This is a wrapper for the cut function. See \code{?cut}. 
#'
#' @return The same data.frame as dafr except that the class 
#' intervals are added as additional column or the the unchanged 
#' dafr data.frame is returned if the input is wrong. Warnings 
#' are provided in this case.
#' 
#' @author Christoph Knapp 
get.form.class.interval = function(dafr,column,num.intervals,
                                   open.left.closed.right=T,
                                   method=c("equal.width",
                                            "equal.count",
                                            "specified"),
                                   intervals=NULL,
                                   labels=NULL){
  if(length(method)>1||!method%in%c("equal.width",
                                    "equal.count",
                                    "specified")){
    method = "equal.width"
  }
  if(!is.null(labels)&&(length(labels)!=num.intervals||
                          any(grepl("^\\s*$",intervals)))){
    warning("The labels not in the right format and are ignored.")
    labels=NULL
  }
  column.temp = dafr[,column]
  ret = dafr
  num.cols.old = ncol(ret)
  if(!is.null(intervals)&&method%in%"specified"){
    if(!is.numeric(intervals)||any(is.na(intervals))||
         is.null(intervals)||length(intervals)!=(num.intervals-1)||
         min(intervals,na.rm=T)<min(dafr[,column],na.rm=T)||
         max(intervals,na.rm=T)<max(dafr[,column],na.rm=T)){
      warning("The \"intervals\" variable is not in the right format.")
      return(dafr)
    }
    column.temp = dafr[,column]
    ret = cbind(dafr,cut(column.temp,
                         breaks=intervals,
                         labels=labels,
                         include.lowest=T,
                         right=open.left.closed.right))
  }else if(method%in%"equal.width"){
    ret = cbind(dafr,cut(x=column.temp, 
                         breaks=num.intervals, 
                         right=open.left.closed.right,
                         labels=labels,
                         include.lowest=TRUE))
  }else if(method%in%"equal.count"){
    ret = cbind(dafr,cut(x=column.temp,
                         breaks=quantile(column.temp, 
                                         probs=seq(0,1,1/num.intervals,),
                                         na.rm=TRUE),
                         include.lowest = TRUE,
                         right = open.left.closed.right,labels=labels))
  }
  if(num.cols.old<ncol(ret)){
    count = 1
    col.name = paste(column,method,count,sep=".")
    while(paste(column,method,count,sep=".")%in%colnames(ret)){
      count = count+1
      col.name = paste(column,method,count,sep=".")
    }
    colnames(ret)[ncol(ret)] = col.name
  }
  ret
}