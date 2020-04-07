### convert matrix element ID to row and column ID
elementToRowCol <- function(element, nrows, ncols){
  row <- ceiling(element/nrows)
  col <- element-(nrows*(row-1))
  return(c(row,col))
}


