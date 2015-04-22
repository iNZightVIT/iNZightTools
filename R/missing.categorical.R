#' Converts specified columns into binary factor variables 
#' which get added to the input data.frame.
#' 
#' @param dafr The input data.frame.
#' @param The column names or indexes to be converted.
#' 
#' @return A data.frame with the binary columns added.
#' 
#' @author Christoph Knapp  
get.missing.categorical = function(dafr,columns){
  temp = as.data.frame(dafr[,columns])
  colnames(temp) = columns
  new.dafr = data.frame(do.call(cbind,lapply(1:length(columns),
                                  function(index,d,c){
                                    te = rep("observed",nrow(d))
                                    te[is.na(d[,index])] = "missing"
                                    te
                                  },temp,columns)))
  colnames(new.dafr) = paste("missing",columns,sep=".")
  cbind(dafr,new.dafr)
}

#' Takes a data.frame as generated from the 
#' \texttt(get.missing.categorical) function and converts it 
#' into a data.frame of all unique rows and there counts in 
#' the original data.
#' 
#' @param dafr A data.frmae to convert
#' 
#' @return A data.frame with all unique rows from dafr and 
#' their counts in the last column.
#' 
#' @author Christoph Knapp
get.combinations = function(dafr){
  ret = data.frame(matrix(ncol=ncol(dafr)+1,nrow=0))
  for(i in 1:nrow(dafr)){
    if(i==1){
      ret = rbind(ret,cbind(as.data.frame(as.matrix(dafr[i,],nrow=1)),1))
    }else{
      is.counted = rep(F,nrow(ret))
      for(j in 1:nrow(ret)){
        if(identical(as.character(unlist(ret[j,1:ncol(dafr)])),as.character(unlist(dafr[i,])))){
          ret[j,ncol(ret)] = ret[j,ncol(ret)]+1
          is.counted[j] = T
        }
      }
      if(all(!is.counted)){
        ret = rbind(ret,cbind(as.data.frame(as.matrix(dafr[i,],nrow=1)),1))
      }
    }
  }
  colnames(ret) = c(colnames(dafr),"counts")
  ret[order(ret$count,decreasing=T),]
}