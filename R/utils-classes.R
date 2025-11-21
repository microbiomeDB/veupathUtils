# helper S4Vectors SimpleList toJSON
# no easy option to be named here really i guess
#' @export
S4SimpleListToJSON <- function(S4SimpleList, named = c(TRUE, FALSE)) {
    if (!inherits(S4SimpleList, 'SimpleList')) stop("S4SimpleListToJSON only accepts an S4Vectors::SimpleList as input.", class(S4SimpleList), "was provided.")

    tmp <- as.list(S4SimpleList)
    tmp <- lapply(tmp, mbioUtils::toJSON, named)
    tmp <- paste(tmp, collapse = ",")
    if (tmp != "") tmp <- paste0("[", tmp, "]")

    return(tmp)
}

#' POSIXct Test
#'
#' This function returns a logical value indicating if x is
#' a POSIXct object.
#' @param x an R object
#' @return logical TRUE if x is a POSIXct object, FALSE otherwise
#' @export
is.POSIXct <- function(x) inherits(x, "POSIXct")

#' @export
new_data_frame <- function(x = list(), n = NULL) {
  if (length(x) != 0 && is.null(names(x))) {
    stop("Elements must be named")
  }
  lengths <- vapply(x, length, integer(1))
  if (is.null(n)) {
    n <- if (length(x) == 0 || min(lengths) == 0) 0 else max(lengths)
  }
  for (i in seq_along(x)) {
    if (lengths[i] == n) next
    if (lengths[i] != 1) {
      stop("Elements must equal the number of rows or 1")
    }
    x[[i]] <- rep(x[[i]], n)
  }

  class(x) <- "data.frame"

  attr(x, "row.names") <- .set_row_names(n)
  x
}

#' Fast data.frame
#' 
#' This function should be used with extreme caution. If data.frame is not 
#' performant enough, you should consider data.table instead. This is 
#' intended to meet a very specific use case where a solution is needed which
#' is both efficient and tidy (meaning tidyverse compatible). It comes at the
#' cost of cutting some corners. There is no checking, recycling etc. unless asked for.
#' @param ... usual arguments to data.frame
#' @export
data_frame <- function(...) {
  new_data_frame(list(...))
}

#' Try-error Test
#'
#' This function returns a logical value indicating if x is
#' a try-error object.
#' @param x an R object
#' @return logical TRUE if x is a try-error object, FALSE otherwise
#' @export
is.error <- function(x) inherits(x, "try-error")


#' Set object attributes from a list
#'
#' This function sets the attributes of an object based on a given list
#' @param .dt data.table
#' @param attr named list of desired attributes to add to .dt
#' @param removeExtraAttrs boolean indicating if existing attributes of .dt not 
#' included in attr should be removed.
#' @export
setAttrFromList <- function(.dt, attr, removeExtraAttrs=T) {
  
  
  if (!data.table::is.data.table(.dt)) {
    stop(".dt must be of class data.table")
  }
  
  # If removeExtraAttrs=T, remove any .dt attribute not in attr
  if (removeExtraAttrs) {
    attrNames <- names(attributes(.dt))
    attrToRemove <- attrNames[!(attrNames %in% names(attr))]
    
    if (length(attrToRemove) > 0) {
      invisible(lapply(attrToRemove, removeAttr, .dt))
    }
  }
  
  # For each item in the attr list, add to .dt attributes or update existing
  invisible(lapply(seq_along(attr), updateAttrById, attr, .dt))
  
  return(.dt)
}

#' @export
removeAttr <- function(attrToRemove, .dt) {
  data.table::setattr(.dt, attrToRemove, NULL)
  return(NULL)
}

#' @export
updateAttrById <- function(attrInd, attr, .dt) {
  data.table::setattr(.dt, names(attr)[attrInd], attr[[attrInd]])
  return(NULL)
}