cHist <-
function(txt) {
  # A wrapper for addHist if the call is already made,
  # and simply needs to be added to the history.
    
    eval(parse(text = addHist(txt)))
}
