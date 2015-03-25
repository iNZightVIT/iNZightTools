#' aggregates the data over selected factor columns
#' 
#' The dimensions of the df data frame will change so that 
#' all possible combinations of factors selected will be 
#' the number of rows and the number of all selected factor 
#' columns + a column for all methods selected will be the 
#' number of columns in the return data.frame. 
#' 
#' @param aggregate.over column names of factor variables 
#' in data to aggregate over
#' @param methods A set of methods which can be used to 
#' aggregate.
#' @param df A data.frame containing at least on factor 
#' column and one numeric column.
#' 
#' @return A data.frame with the results of the aggregation.
#' 
#' @author Christoph Knapp
#'
#' 
aggregate.data= function(aggregate.over,
               methods=c("mean","median","sum","sd","IQR","count"),
               dafr){
  if(is.null(aggregate.over)|is.null(methods)|length(methods)==0|
       length(aggregate.over)==0|is.numeric(aggregate.over)){
    stop("aggregate.data : Wrong input")
  }
  if(any(!as.character(aggregate.over)%in%colnames(dafr))){
    warning("aggregate.data : Some columns in aggregate.over are 
            not in the column names for df. They will be ignored.")
  }
  if(is.character(aggregate.over)){
    aggregate.over = as.factor(aggregate.over)
  }
  bys = lapply(1:length(aggregate.over),
               function(i,d,v){
                 d[,which(colnames(d)%in%v[i])]
               },dafr,aggregate.over)
  names(bys) = aggregate.over
  sets = lapply(1:length(methods),function(i,d,b,m){
    if("count"%in%m[i]){
      m[i] = "length"
      temp = aggregate(d[,unlist(lapply(1:ncol(d),
                                        function(j,da){
                                          is.numeric(da[,j])
                                        },d))], 
                       by=b, FUN=m[i],simplify = FALSE)
    }else{
      temp = aggregate(d[,unlist(lapply(1:ncol(d),
                                        function(j,da){
                                          is.numeric(da[,j])
                                        },d))], 
                       by=b, FUN=m[i],na.rm=T,simplify = FALSE)
    }
    
    colnames(temp)[(length(b)+1):ncol(temp)] = paste(m[i],
                                                     colnames(temp)[(length(b)+1):ncol(temp)],sep=".")
    temp
    },dafr,bys,methods)
  temp = sets[[1]]
  if(length(sets)>1){
    for(i in 2:length(sets)){
      temp = merge(temp,sets[[i]])
    }
  }
  temp
}