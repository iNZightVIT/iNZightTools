#' Takes a sample of rows from a data.frame
#' 
#' This function samples rows from a data.frame with or 
#' without replacment. 
#' 
#' @param df A data.frame the sample is taken from.
#' @param sampleSize The size of the samples to be taken.
#' @param numSample The number of samples to be taken.
#' @param bootstrap TRUE if samples with replacement is 
#' desired, FALSE if no replacement.
#' 
#' @return A data.frame with the samples merged together. 
#' An additional column is added where the sampling 
#' iteration is stored. 
#' 
#' @author Christoph Knapp
#' 
#' @export
sample.data = function(df,sampleSize,numSample=1,bootstrap=F){
  if(sampleSize>nrow(df)){
    stop(paste0("This sample is to large. Only ",nrow(df)," samples available."))
  }
  if(sampleSize*numSample>nrow(df)&!bootstrap){
    stop(paste0("Not enough rows in data to sample that many times."))
  }
  colname = "num.sample"
  if("num.sample"%in%colnames(df)){
    count=1
    while(paste0("num.sample",count%in%colnames(df))){
      count = count++
    }
    colname = paste0("num.sample",count)
  }
  ret = NULL
  if(bootstrap){
    ret = do.call(rbind(lapply(1:numSample,function(index,d,size){
      cbind(d[sample(1:nrow(d),size),],rep(index,size))
    },df,sampleSize)))
  }else{
    ret = do.call(rbind,lapply(1:numSample,function(index,d,size){
      s = sample(1:nrow(d),size)
      temp = cbind(d[s,],rep(index,size))
      df <<- df[-s,]
      temp
    },data,sampleSize))
  }
  colnames(ret)[ncol(ret)] = colname
  ret
}