### main function
tidy_code=function(codeline,width,indent){
  code=getcode(codeline)
  indents=getindents(code)
  codeList <- list()
  for (i in seq_along(1:length(code))) {
    codeList[[i]] <- txtCodeString(code[i], indents[i])
  }
  final <- list()
  cl=makeCodeList(codeList)
  return (sapply(cl,printcode6,wi=width,i=indent))
}

tidy_all_code=function(messy_code,inclLib,width,indent,outfile){
  if (length(messy_code) == 1 && file.exists(messy_code)){
    allcode=getText(messy_code,inclLib)
    strvector=sapply(allcode, tidy_code,width=2,indent=2)
    splist=strsplit(strvector,"\n")
    splist=unlist(splist,use.names = FALSE)
    splist=splist[splist!=""]
    write(splist,outfile)
  }
}

### import txt file and library names can display or not display
getText=function(x,inclLib=TRUE){
  code=readLines(x)
  code1=code[trimws(code)!=""]
  if(inclLib){
    code1=code1
  }else{
    code2=vector()
    for (i in (1:length(code1))){
      withpn=code1[i]
      package = gregexpr("::",withpn)
      if(!(package %in% -1)){
        space = gregexpr(" ",withpn)
        
        allname=vector()
        for (i in (1:length(package[[1]]))){
          allname=c(allname,substr(withpn,space[[1]][max(which(space[[1]]<package[[1]][i]))]+1,package[[1]][i]+1))
        }
        allname=paste0(allname,collapse = "|")
        without=gsub(allname,"",withpn)
      }else{without=code1[i]}
      code2=c(code2,without)
    }
    code1=code2[trimws(code2)!=""]
  }
  for (i in (1:length(code1))){
    if(i<length(code1)&&grepl("<- $",code1[i])){
      code1[i]=paste0(code1[i],code1[i+1])
      code1[i+1]=""
    }
  }
  code1=code1[code1!=""]
  
  opindex=sapply(code1, getop)
  
  varlist=sapply(code1, getvariable)
  
  
  for(i in length(varlist):1){
    if(i>1&&varlist[[i]]==varlist[[i-1]]){
      code1[i-1]=paste(code1[i-1],"%<>%",substring(code1[i],opindex[[i]]+1,nchar(code1[i])))
      code1[i]=""
    }
  }
  
  return (code1[code1!=""])
}

getvariable=function(code){
  pos=regexpr("%<>%|<-|%>%",code)
  variable=substr(code,1,pos[[1]]-1)
  variable=trimws(variable)
  variable
}
getop=function(code){
  pos=regexpr("%<>%|<-|%>%",code)
  pos[[1]]+attr(pos,"match.length")-1
}

###separating codes to be in each new line
getcode = function(code){
  exp="\\(|<-|%<>%|%>%|=|,"
  br=gregexpr(exp,code)
  length=attr(br[[1]],"match.length")
  length[which(length==1)]=0
  split=substring(code,c(1,(br[[1]]+length+1)),c((br[[1]]+length),nchar(code)))
  cbr=gregexpr("\\)|\\),",split)
  new=vector()
  for (i in (1:length(cbr))){
    if(!any(cbr[[i]]==-1)){
      new=substring(split[i],c(1,cbr[[i]]),c(cbr[[i]]-1,nchar(split[i])))
      split[i]=list(new)
    }else{next}
  }
  codevector=trimws(unlist(split))
  codevector[codevector!=""]
}

###getting the corresponding indents for the above codes
getindents=function(code1){
  parind=integer(length(code1))
  ind=0
  LbrPos=grep("\\(",code1)
  if(length(LbrPos)==0){
    firstbr=0
    LbrPos=0
  }else{firstbr=LbrPos[1]}
  for (i in 1:length(code1)){
    
    if((!grepl("\\)",code1[i]))&&grepl("<-|\\(|%<>%|%>%|=",code1[i])){
      ind=ind+1
      parind[i+1]=ind
      
    }else{
      if(grepl("\\)|\\),",code1[i])){
        indLbr=max(LbrPos[LbrPos<i])
        parind[i]=parind[indLbr]
        pos=match(indLbr,LbrPos)
        LbrPos=LbrPos[-pos]
        parind[i+1]=parind[i]
        ind=parind[i+1]
        
      }
      else{
        if(i>=firstbr&&all(LbrPos>i)){
          ind=parind[firstbr]
          parind[i+1]= ind
        }else{parind[i+1]=ind}
      }
    }
    
  }
  totInd=parind[1:length(code1)]
  totInd
}

### helping functions 
txtCodeString <- function(code, ind) {
  z <- list(text = code, indent = ind)
  class(z) <- "txtcodestring"
  z
}

print.txtcodestring <- function(x, ...) {
  # cat(x$indent, ": ", x$text, "\n", sep = "")
  cat(sep = "",
      #x$indent, ": ",
      rep(" ", x$indent*4), x$text, "\n")
  
  if (!is.null(x$subcode)) {
    # cat("---\n")
    sapply(x$subcode, print)
    # cat("+++\n")
  }
}


makeCodeList <- function(cl) {
  this <- cl[[1]]
  i0 <- this$indent
  sibs <- which(sapply(cl, function(x) x$indent == i0))
  
  N <- length(cl)
  sibs <- c(sibs, N+1)
  final <- list()
  for (i in seq_along(1:(length(sibs)-1))) {
    final[i] <- cl[sibs[i]]
    if (sibs[i] < N) {
      wi <- sibs[i] : (sibs[i + 1] - 1)
      if (length(wi) > 1)
        final[[i]]$subcode <- makeCodeList(cl[ wi[-1] ])
    }
    
  }
  final
}

codelength=function(cl){
  if(length(cl$subcode)>0){
    len=nchar(cl$text)+sum(sapply(cl$subcode,codelength))
  }else{len=nchar(cl$text)}
  len
}

cancollapse <- function(cs, width = 0, indent=2) {
  if(length(cs$subcode)>0){
    for (i in 1:length(cs$subcode)) {
      wi = width - indent
      if (i < length(cs$subcode) && grepl("\\(",cs$subcode[[i]]$text)&& grepl("\\)",cs$subcode[[i+1]]$text)){ 
        wi = wi - length(cs$subcode[[i+1]])
      }
      if (!(cancollapse(cs$subcode[[i]], width = wi))) return(FALSE)
    }
    
  }
  codelength(cs)<=width
}

### printing code with correct indents and within defined width 
printcode6=function(x,wi,previous=FALSE,ind=TRUE,noind=FALSE,i=2){
  longstr=""
  if(length(x$subcode)>0){
    if(cancollapse(x,width=wi)){
      if(previous){
        longstr=do.call(paste,c(x$text,lapply(x$subcode,printcode6,wi=wi,previous=TRUE,ind=FALSE,noind=FALSE))) 
      }else{
        longstr=do.call(paste,c(paste(paste(rep(" ",(x$indent*i)),collapse=""),x$text,sep=""),
                                lapply(x$subcode,printcode6,wi=wi,previous=TRUE,ind=TRUE,noind=FALSE)))
      }
    }
    
    #cannot collapse
    else{
      
      longstr=paste(paste(rep(" ",(x$indent*i)),collapse=""),x$text,"\n",sep="")
      for (i in 1:length(x$subcode)){
        if(i<length(x$subcode)&&grepl("\\)",x$subcode[[i+1]]$text)){
          
          longstr=paste(longstr,
                        printcode6(x$subcode[[i]],wi=wi-nchar(x$subcode[[i+1]]$text)-x$subcode[[i]]$indent,previous=FALSE,ind=TRUE,noind = FALSE),
                        sep="")
          
          
        }else{
          
          if(i>1&&grepl("\\(",x$subcode[[i-1]]$text)&&grepl("\\)",x$subcode[[i]]$text)&&
             cancollapse(x$subcode[[i-1]],width=wi-nchar(x$subcode[[i]]$text)-x$subcode[[i]]$indent)){
            
            longstr=paste(longstr,printcode6(x$subcode[[i]],wi=wi-x$subcode[[i]]$indent,previous=FALSE,ind=TRUE,noind=TRUE),sep="")
          }
          
          else{ 
            longstr=paste(longstr,
                          printcode6(x$subcode[[i]],wi=wi-x$subcode[[i]]$indent,previous=FALSE,ind=TRUE,noind = FALSE),sep="")}}
      }
    }
    
  }else{
    if(ind&&previous){
      if(grepl("\\)|%>%",x$text)){longstr=paste(x$text)}
      else{longstr=paste(x$text,sep="")}
    }
    if(!previous&&ind){
      if(noind){longstr=paste(x$text,"\n",sep="")}
      else{longstr=paste("\n",paste(rep(" ",(x$indent*i)),collapse=""),x$text,"\n",sep="")}
    }
    if(previous&&!ind){
      longstr=paste(x$text)
    }
  }
  longstr
}