addHist <-
function(txt) {
  # This simple function adds text (a function call)
  # to the history object, and returns the call.
    
    e$hist <<- c(e$hist, txt)
    txt
}
