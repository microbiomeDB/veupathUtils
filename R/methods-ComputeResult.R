#' Write Computed Variable Metadata
#'
#' This function, for any given ComputeResult, 
#' will return a filehandle where ComputedVariableMetadata
#' has been written in JSON format.
#' 
#' @param object ComputeResult 
#' @param pattern string to incorporate into tmp file name
#' @param verbose boolean indicating if timed logging is desired
#' @return filehandle where JSON representation of ComputedVariableMetadata can be found
#' @export
setGeneric("writeMeta",
  function(object, pattern = NULL, verbose = c(TRUE, FALSE)) standardGeneric("writeMeta"),
  signature = c("object")
)

#'@export 
setMethod("writeMeta", signature("ComputeResult"), function(object, pattern = NULL, verbose = c(TRUE, FALSE)) {
  verbose <- mbioUtils::matchArg(verbose)

  outJson <- mbioUtils::toJSON(object@computedVariableMetadata)

  if (is.null(pattern)) { 
    pattern <- object@name
    if (is.null(pattern)) {
      pattern <- 'file'
    } 
  }
  pattern <- paste0(pattern, '-meta-')

  outFileName <- basename(tempfile(pattern = pattern, tmpdir = tempdir(), fileext = ".json"))
  write(outJson, outFileName)
  mbioUtils::logWithTime(paste('New output file written:', outFileName), verbose)

  return(outFileName)
})

#' Write Compute Result Data
#'
#' This function, for any given ComputeResult, 
#' will return a filehandle where result data
#' has been written in tab delimited format.
#' @param object ComputeResult 
#' @param pattern string to incorporate into tmp file name
#' @param verbose boolean indicating if timed logging is desired
#' @return filehandle where tab delimited representation of result data can be found
#' @export
setGeneric("writeData",
  function(object, pattern = NULL, verbose = c(TRUE, FALSE)) standardGeneric("writeData"),
  signature = c("object")
)

#'@export 
setMethod("writeData", signature("ComputeResult"), function(object, pattern = NULL, verbose = c(TRUE, FALSE)) {
  verbose <- mbioUtils::matchArg(verbose)

  if (is.null(pattern)) { 
    pattern <- object@name
    if (is.null(pattern)) {
      pattern <- 'file'
    } 
  }
  pattern <- paste0(pattern, '-data-')

  outFileName <- basename(tempfile(pattern = pattern, tmpdir = tempdir(), fileext = ".tab"))
  data.table::fwrite(object@data, outFileName, sep = '\t', quote = FALSE)
  mbioUtils::logWithTime(paste('New output file written:', outFileName), verbose)

  return(outFileName)
})




#' Write Computed Variable Statistics
#'
#' This function, for any given ComputeResult, 
#' will return a filehandle where the statistics table
#' has been written in JSON format.
#' 
#' @param object ComputeResult with statistics
#' @param pattern string to incorporate into tmp file name
#' @param verbose boolean indicating if timed logging is desired
#' @return filehandle where JSON representation of ComputeResult statistics can be found
#' @export
setGeneric("writeStatistics",
  function(object, pattern = NULL, verbose = c(TRUE, FALSE)) standardGeneric("writeStatistics"),
  signature = c("object")
)

#'@export 
setMethod("writeStatistics", signature("ComputeResult"), function(object, pattern = NULL, verbose = c(TRUE, FALSE)) {
  verbose <- mbioUtils::matchArg(verbose)

  # Convert all to character but maintain structure
  if (inherits(object@statistics, 'data.frame')) {
    outObject <- data.frame(lapply(object@statistics, as.character))
  } else {
    outObject <- object@statistics
  }

  outJson <- jsonlite::toJSON(outObject)

  if (is.null(pattern)) { 
    pattern <- object@name
    if (is.null(pattern)) {
      pattern <- 'file'
    } 
  }
  pattern <- paste0(pattern, '-statistics-')

  outFileName <- basename(tempfile(pattern = pattern, tmpdir = tempdir(), fileext = ".json"))
  write(outJson, outFileName)
  mbioUtils::logWithTime(paste('New output file written:', outFileName), verbose)

  return(outFileName)
})

#' @include methods-VariableMetadata.R
setMethod("toJSON", signature("CorrelationResult"), function(object, ...) {
  tmp <- character()

  # The outputted CorrelationResult json object should have properties data1Metadata, data2Metadata, and statistics.
  # All values in statistics should be strings.
  tmp <- paste0(tmp, '"data1Metadata": ', jsonlite::toJSON(jsonlite::unbox(object@data1Metadata)), ',')
  tmp <- paste0(tmp, '"data2Metadata": ', jsonlite::toJSON(jsonlite::unbox(object@data2Metadata)), ',')
  outObject <- data.frame(lapply(object@statistics, as.character))
  tmp <- paste0(tmp, paste0('"statistics": ', jsonlite::toJSON(outObject)))

  tmp <- paste0("{", tmp, "}")
  return(tmp)
})

# these let jsonlite::toJSON work by using the custom toJSON method for our custom result class
asJSONGeneric <- getGeneric("asJSON", package = "jsonlite")
setMethod(asJSONGeneric, "CorrelationResult", function(x, ...) toJSON(x))