#' @rdname getSampleMetadata
#' @aliases getSampleMetadata,SampleMetadata-method 
setMethod("getSampleMetadata", signature("SampleMetadata"), function(object, asCopy = c(TRUE, FALSE), includeIds = c(TRUE, FALSE)) {
  asCopy <- mbioUtils::matchArg(asCopy)
  includeIds <- mbioUtils::matchArg(includeIds)
  dt <- object@data

  # Check that incoming dt meets requirements
  if (!inherits(dt, 'data.table')) {
    data.table::setDT(dt)
  }

  if (asCopy) {
    dt <- data.table::copy(dt)
  }

  if (!includeIds) {
    allIdColumns <- c(object@recordIdColumn, object@ancestorIdColumns)
    dt <- dt[, -..allIdColumns]
  }

  return(dt)
})

#' @rdname getSampleMetadataIdColumns
#' @aliases getSampleMetadataIdColumns,SampleMetadata-method
setMethod("getSampleMetadataIdColumns", "SampleMetadata", function(object) getIdColumns(object))