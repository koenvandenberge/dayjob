### convert matrix element ID to row and column ID
elementToRowCol <- function(element, nrows, ncols){
  row <- ceiling(element/nrows)
  col <- element-(nrows*(row-1))
  return(c(row,col))
}


### color by factor or numeric variable using wesanderson colors
colby <- function(values, g=12){
  if(is(values, "character")){
    cols <- as.numeric(as.factor(values))
    return(cols)
  }
  if(is(values, "factor")){
    cols <- as.numeric(values)
    return(cols)
  }
  if(is(values, "numeric")){
    gg <- Hmisc::cut2(values, g=g)
    pal <- wesanderson::wes_palette("Zissou1", n=nlevels(gg), type="continuous")
    cols <- pal[gg]
    return(cols)
  }
  if(is(values, "logical")){
    cols <- as.numeric(values)+1
    return(cols)
  }
}

## drop NA values from a vector
dropNA <- function(x){
  x <- x[!is.na(x)]
}
