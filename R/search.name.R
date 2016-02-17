#' Searches a list recursivly for a name and
#' returns the value associated with the name
#' 
#' @param list.search A list to search
#' @param search.name A name to be found in a list. 
#' If null, the whole simplified list is returned.
#' 
#' @return The value found in the list associated 
#' with search.name, NULL if not there.
#' 
#' @details This function is used to search the 
#' everchanging plot output from iNZightPlot. 
#' It can handle 4 classes found in the plot 
#' output (list, inzplotoutput, inzgrid,
#' inzpar.list) which can be treated as list 
#' type object. Some of the objects retrieved 
#' might also be a list. This happens when the 
#' name to search for is found several times in 
#' the nested list structure of the plot output.
#' 
#' @author Christoph Knapp
#' 
#' @export
search.name = function(list.search,search.name=NULL){
  list.out=list()
  search = function(input.list,nam=NULL){
    if("list"%in%class(input.list)||
         "inzplotoutput"%in%class(input.list)||
         "inzgrid"%in%class(input.list)||
         "inzpar.list"%in%class(input.list)||
         "inzdot"%in%class(input.list)||
         "inzhist"%in%class(input.list)||
         "inzscatter"%in%class(input.list)||
         "inzbar"%in%class(input.list)){
      for(i in 1:length(input.list)){
        if(!is.null(names(input.list)[i])){
          nam = names(input.list)[i]
        }else{
          nam = i
        }
        search(input.list[[nam]],nam)
      }
    }else{
      if(is.null(nam)){
        nam=length(list.out)+1
      }
      if(!is.null(input.list)){
        if(nam%in%names(list.out)){
          temp = list.out[[nam]]
          list.out[[nam]][[length(list.out[[nam]])+1]] <<- input.list
        }else{
          list.out[[nam]] <<- list(input.list)
        }
      }
    }
  }
  search(list.search)
  if(is.null(search.name)){
    list.out
  }else{
    list.out[[search.name]]
  }
}
