#' Sorts the df data frame after the input variables
#' 
#' Warnings will be given if the colnames in vars will not 
#' match the column in df. This is a wrapper function for 
#' sort. See \code{?sort} for more information.
#' 
#' @param vars The column names in the order the df data.frame
#' should be sorted.
#' @param sort.type A logical vector of the same length as 
#' vars. If the element in the vector is TRUE the corresponding 
#' element in vars will be sorted in increasing order.
#' @ param The data.fram or matrix to sort.
#' 
#' @return An ordered data.frame
#' 
#' @author Christoph Knapp  
sort.data = function(vars,sort.type,df){
  if(any(!vars%in%colnames(df))){
    warning("sort.data : Not all variables in vars could be 
            matched to column names in df.")
  }
  if(length(vars)!=length(sort.type)){
    stop("sort.data : vas and sort.type have different length")
  }
  z = lapply(1:length(vars),function(index,v,t,d){
    vec = d[,which(colnames(d)%in%v[index])]
    if(is.factor(vec)|is.character(vec)){
      vec = xtfrm(as.character(vec))
    }
    if(!t[index]){
      vec = -vec
    }
    vec
  },vars,sort.type,df)
  df[order.overwrite(z),]
}

#' The iNZight version of the order function which lets you pass 
#' in a list of vectors to order instead of the ... argument. It 
#' is shortened and might be therefore not as stable as the 
#' original order function.
#' 
#' @param z a sequence of numeric, complex, character or logical 
#' vectors, all of the same length, or a classed R object.
#' @param na.last for controlling the treatment of NAs. If TRUE, 
#' missing values in the data are put last; if FALSE, they are 
#' put first; if NA, they are removed (see ‘Note’.)
#' @param decreasing logical. Should the sort order be increasing 
#' or decreasing?
#' 
#' @note This function is only called in sort.data but needs to be 
#' available to sort.data
#' 
#' @author Christoph Knapp
order.overwrite = function (z, na.last = TRUE, decreasing = FALSE) {
  if (any(diff(l.z <- vapply(z, length, 1L)) != 0L)) 
    stop("argument lengths differ")
  ans <- vapply(z, is.na, rep.int(NA, l.z[1L]))
  ok <- if (is.matrix(ans)) 
    !apply(ans, 1, any)
  else !any(ans)
  if (all(!ok)) 
    return(integer())
  z[[1L]][!ok] <- NA
  ans <- do.call("order", c(z, decreasing = decreasing))
  keep <- seq_along(ok)[ok]
  ans[ans %in% keep]
}