#' Find numeric columns
#'
#' This function finds all numeric columns in a data structure
#' @param x list, data.frame, data.table, or vector
#' @return vector of numeric column names in x. If no numeric columns found, returns NULL
#' @export
findNumericCols <- function(x) {
  numericCols <- names(x)[sapply(x, is.numeric)]
  
  # If no numeric cols, return NULL
  if (!length(numericCols)) return(NULL)
  
  return(numericCols)
}
