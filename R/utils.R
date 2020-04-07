### convert matrix element ID to row and column ID
elementToRowCol <- function(element, nrows, ncols){
  row <- ceiling(element/nrows)
  col <- element-(nrows*(row-1))
  return(c(row,col))
}


### color by factor or numeric variable using wesanderson colors
colby <- function(values){
  if(is(values, "character")){
    cols <- as.numeric(as.factor(values))
    return(cols)
  }
  if(is(values, "factor")){
    cols <- as.numeric(values)
    return(cols)
  }
  if(is(values, "numeric")){
    pal <- wesanderson::wes_palette("Zissou1", n=12, type="continuous")
    gg <- Hmisc::cut2(values, g=12)
    cols <- pal[gg]
    return(cols)
  }
}
