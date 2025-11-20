#' Predicate Factory
#' 
#' This function creates a predicate function based on a string defining the type of predicate to run and a numeric value. 
#' The currently supported types are 'proportionNonZero', 'variance' and 'sd'. The numeric value associated
#' with each predicate type is the threshold for the predicate to be true.
#' 
#' @param predicateType string defining the type of predicate to run. 
#' The currently supported values are 'proportionNonZero', 'variance' and 'sd'
#' @param threshold numeric value associated with the predicate type
#' @return Function returning a boolean indicating if a feature should be included (TRUE) or excluded (FALSE)
#' @export
setGeneric("predicateFactory",
  function(predicateType, threshold) standardGeneric("predicateFactory"),
  signature = c("predicateType", "threshold")
)


#' Predicate Factory
#'
#' This function creates a predicate function based on a string defining the type of predicate to run and a numeric value. 
#' The currently supported types are 'proportionNonZero', 'variance' and 'sd'. The numeric value associated
#' with each predicate type is the threshold for the predicate to be true.
#' 
#' @param predicateType string defining the type of predicate to run. 
#' The currently supported values are 'proportionNonZero', 'variance' and 'sd'
#' @param threshold numeric value associated with the predicate type
#' @return Function returning a boolean indicating if a feature should be included (TRUE) or excluded (FALSE)
#' @export
setMethod("predicateFactory", signature("character", "numeric"), 
function(
  predicateType = c('proportionNonZero', 'variance', 'sd'), 
  threshold = 0.5
) {
  predicateType <- mbioUtils::matchArg(predicateType)

  if (predicateType == 'proportionNonZero') {
    if (threshold < 0 | threshold > 1) {
      stop('threshold must be between 0 and 1 for proportionNonZero')
    }
    return(function(x){sum(x > 0) >= length(x) * threshold})

  } else if (predicateType == 'variance') {
    if (threshold < 0) {
      stop('threshold must be greater than 0 for variance')
    }
    return(function(x){var(x) > threshold})

  } else if (predicateType == 'sd') {
    if (threshold < 0) {
      stop('threshold must be greater than 0 for sd')
    }
    return(function(x){sd(x) > threshold})
  }

})

## look at this cool trick i learned
setClassUnion("missingOrNULL", c("missing", "NULL"))

#' Correlation
#'
#' This function returns correlation coefficients for variables in one dataset against variables in a second dataset
#' 
#' @param data1 first dataset. A data.table
#' @param data2 second dataset. A data.table
#' @param method string defining the type of correlation to run. 
#' The currently supported values are specific to the class of data1 and data2.
#' @param format string defining the desired format of the result. 
#' The currently supported values are 'data.table' and 'ComputeResult'.
#' @param verbose boolean indicating if timed logging is desired
#' @return data.frame with correlation coefficients or a ComputeResult object
#' @import data.table
#' @rdname correlation
#' @export
setGeneric("correlation",
  function(
    data1, 
    data2, 
    method, 
    format = c('ComputeResult', 'data.table'), 
    verbose = c(TRUE, FALSE), 
    ...
  ) standardGeneric("correlation"),
  signature = c("data1","data2")
)

############ These methods should be used with caution. It is recommended to build methods for specific classes instead. ############
############          That allows for proper validation of inputs (sparcc is for compositional data for example).        ############
############                                 See microbiomeComputations for an example.                                  ############

#' @importFrom Hmisc rcorr
#' @return data.frame with correlation coefficients
#' @rdname correlation
#' @aliases correlation,data.table,data.table-method
setMethod("correlation", signature("data.table", "data.table"), 
function(
  data1, 
  data2, 
  method = c('spearman','pearson'), 
  format = c('ComputeResult', 'data.table'), 
  verbose = c(TRUE, FALSE)
) {

  format <- mbioUtils::matchArg(format)
  method <- mbioUtils::matchArg(method)
  verbose <- mbioUtils::matchArg(verbose)

  # Check that the number of rows match.
  if (!identical(nrow(data1), nrow(data2))) {
    stop("data1 and data2 must have the same number of rows.")
  }

  # Check that all values are numeric
  if (!identical(mbioUtils::findNumericCols(data1), names(data1))) { 
    warning("All columns in data1 are not numeric. Only numeric columns will be used.")
    keepCols <- mbioUtils::findNumericCols(data1)
    if (length(keepCols) == 0) {
      stop("No numeric columns found in data1.")
    }
    data1 <- data1[, ..keepCols]
  }
  if (!identical(mbioUtils::findNumericCols(data2), names(data2))) { 
    warning("All columns in data2 are not numeric. Only numeric columns will be used.")
    keepCols <- mbioUtils::findNumericCols(data2)
    if (length(keepCols) == 0) {
      stop("No numeric columns found in data2.")
    }
    data2 <- data2[, ..keepCols]
  }


  ## Compute correlation
  #corrResult <- data.table::as.data.table(cor(data1, data2, method = method, use='na.or.complete'), keep.rownames = T)
  lastData1ColIndex <- length(data1)
  firstData2ColIndex <- length(data1) + 1
  corrResult <- Hmisc::rcorr(as.matrix(data1), as.matrix(data2), type = method)
  # this bc Hmisc::rcorr cbinds the two data.tables and runs the correlation
  # so we need to extract only the relevant values
  # of note, seems subsetting a matrix to have a single row/ column drops rownames by default. adding drop=F to prevent this.
  pVals <- data.table::as.data.table(
    corrResult$P[1:lastData1ColIndex, firstData2ColIndex:length(colnames(corrResult$P)), drop = F], 
    keep.rownames = T
  )
  corrResult <- data.table::as.data.table(
    corrResult$r[1:lastData1ColIndex, firstData2ColIndex:length(colnames(corrResult$r)), drop = F], 
    keep.rownames = T
  )

  mbioUtils::logWithTime(paste0('Completed correlation with method=', method,'. Formatting results.'), verbose)


  ## Format results
  meltedCorrResult <- melt(corrResult, id.vars=c('rn'))
  meltedPVals <- melt(pVals, id.vars=c('rn'))
  formattedCorrResult <- data.frame(
    data1 = meltedCorrResult[['rn']],
    data2 = meltedCorrResult[['variable']],
    correlationCoef = meltedCorrResult[['value']],
    # should we do a merge just to be sure?
    pValue = meltedPVals[['value']]
  )

  if (format == 'data.table') {
    return(formattedCorrResult)
  } else {
    result <- buildCorrelationComputeResult(formattedCorrResult, data1, data2, method, verbose)
    result@computationDetails <- 'correlation'
    return(result)
  }
})


#' @importFrom SpiecEasi pval.sparccboot
#' @importFrom SpiecEasi sparccboot
#' @importFrom SpiecEasi sparcc
#' @rdname correlation
#' @aliases correlation,data.table,missingOrNULL-method
setMethod("correlation", signature("data.table", "missingOrNULL"), 
function(
  data1, 
  data2, 
  method = c('spearman','pearson','sparcc'), 
  format = c('ComputeResult', 'data.table'), 
  verbose = c(TRUE, FALSE)
) {

  format <- mbioUtils::matchArg(format)
  method <- mbioUtils::matchArg(method)
  verbose <- mbioUtils::matchArg(verbose)

  # Check that all values are numeric
  if (!identical(mbioUtils::findNumericCols(data1), names(data1))) { 
    warning("All columns in data1 are not numeric. Only numeric columns will be used.")
    keepCols <- mbioUtils::findNumericCols(data1)
    if (length(keepCols) == 0) {
      stop("No numeric columns found in data1.")
    }
    data1 <- data1[, ..keepCols]
  }

  ## Compute correlation
  # rownames and colnames should be the same in this case
  # keep matrix for now so we can use lower.tri later, expand.grid will give us the needed data.frame
  if (method == 'sparcc') {

    # this is a local alias for the sparcc function from the SpiecEasi namespace that do.call can find
    # if we need to customize how we call sparcc in the future, we'll need to do something like the following
    # except the empty list would have args for the sparcc function
    #sparcc <- get("sparcc", asNamespace("SpiecEasi"))
    #statisticperm=function(data, indices) do.call("sparcc", c(list(apply(data[indices,], 2, sample)), list()))$Cor
    #statisticboot=function(data, indices) do.call("sparcc", c(list(data[indices,,drop=FALSE]), list()))$Cor

    # sub-sampled `statistic` functions to pass to `boot::boot` that we can use to find pvalues
    # statisticboot = function which takes data and bootstrap sample indices of the bootstapped correlation matrix
    # statisticperm = function which takes data and permutated sample indices of the null correlation matrix
    # the SpiecEasi defaults for these functions return the upper triangle of the correlation matrix. We do that manually later instead.
    statisticperm = function(data, indices) SpiecEasi::sparcc(apply(data[indices,], 2, sample))$Cor
    statisticboot = function(data, indices) SpiecEasi::sparcc(data[indices,,drop=FALSE])$Cor

    # calling the bootstrap version of sparcc and finding pvalues
    result <- SpiecEasi::pval.sparccboot(SpiecEasi::sparccboot(data1, statisticboot = statisticboot, statisticperm = statisticperm, R = 100))
    # making sure results are formatted correctly for downstream use
    pVals <- matrix(result$pvals, nrow = ncol(data1), ncol = ncol(data1), byrow = T)
    corrResult <- result$cors
    rownames(corrResult) <- colnames(corrResult) <- colnames(data1)
  
  } else {
  
    corrResult <- Hmisc::rcorr(as.matrix(data1), type = method)
    pVals <- corrResult$P
    corrResult <- corrResult$r
  
  }

  mbioUtils::logWithTime(paste0('Completed correlation with method=', method,'. Formatting results.'), verbose)

  ## Format results
  rowAndColNames <- expand.grid(rownames(corrResult), colnames(corrResult))
  deDupedRowAndColNames <- rowAndColNames[as.vector(upper.tri(corrResult)),]
  formattedCorrResult <- cbind(deDupedRowAndColNames, corrResult[upper.tri(corrResult)])
  formattedCorrResult <- cbind(formattedCorrResult, pVals[upper.tri(pVals)])
  colnames(formattedCorrResult) <- c("data1","data2","correlationCoef","pValue")

  if (format == 'data.table') {
    return(formattedCorrResult)
  } else {
    result <- buildCorrelationComputeResult(formattedCorrResult, data1, data2, method, verbose)
    result@computationDetails <- 'selfCorrelation'
    return(result)
  }
})

getDataMetadataType <- function(data) {
  if (inherits(data, 'CollectionWithMetadata')) {
    return('assay')
  } else if (inherits(data, 'SampleMetadata')) {
    return('sampleMetadata')
  } else {
    return('unknown')
  }
}

## Helper function
# should this be s4?
#' @export
buildCorrelationComputeResult <- function(
  corrResult, 
  data1, 
  data2 = NULL, 
  method = c('spearman','pearson','sparcc'), 
  verbose = c(TRUE, FALSE)
) {
  method <- mbioUtils::matchArg(method)
  verbose <- mbioUtils::matchArg(verbose)

  # both AbundanceData and SampleMetadata have these slots
  recordIdColumn <- ifelse('recordIdColumn' %in% slotNames(data1), data1@recordIdColumn, NA_character_)
  ancestorIdColumns <- ifelse('ancestorIdColumns' %in% slotNames(data1), data1@ancestorIdColumns, NA_character_)
  allIdColumns <- c(recordIdColumn, ancestorIdColumns)

  ## Format results
  # Construct the ComputeResult
  result <- new("ComputeResult")
  result@name <- 'correlation'
  result@recordIdColumn <- recordIdColumn
  result@ancestorIdColumns <- ancestorIdColumns
  statistics <- CorrelationResult(
    statistics = corrResult,
    data1Metadata = getDataMetadataType(data1),
    data2Metadata = ifelse(is.null(data2), getDataMetadataType(data1), getDataMetadataType(data2))
  )
  result@statistics <- statistics
  result@parameters <- paste0('method = ', method)

  validObject(result)
  mbioUtils::logWithTime(
    paste(
      'Correlation computation completed with parameters recordIdColumn=', recordIdColumn, 
      ', method = ', method
    ), 
    verbose
  )
  
  return(result)
}

#' Self Correlation
#'
#' This function returns correlation coefficients for variables in one dataset against itself
#' 
#' @param data A data.table or Collection object.
#' @param method string defining the type of correlation to run. 
#' The currently supported values are 'spearman','pearson', 
#' and for some methods/ data types 'sparcc'.
#' @param format string defining the desired format of the result. 
#' The currently supported values are 'data.table' and 'ComputeResult'.
#' @param verbose boolean indicating if timed logging is desired
#' @return ComputeResult object
#' @rdname selfCorrelation
#' @export
setGeneric("selfCorrelation",
  function(
    data, 
    method = c('spearman','pearson','sparcc'), 
    format = c('ComputeResult', 'data.table'), 
    verbose = c(TRUE, FALSE), 
    ...
  ) standardGeneric("selfCorrelation"),
  signature = c("data")
)

#' @rdname selfCorrelation
#' @aliases selfCorrelation,data.table-method
setMethod("selfCorrelation", signature("data.table"), 
function(
  data, 
  method = c('spearman','pearson'), 
  format = c('ComputeResult', 'data.table'), 
  verbose = c(TRUE, FALSE)
) {

  format <- mbioUtils::matchArg(format)
  method <- mbioUtils::matchArg(method)
  verbose <- mbioUtils::matchArg(verbose)
  
  correlation(data, NULL, method = method, format = format, verbose = verbose)
})

#' @rdname correlation
#' @aliases correlation,CollectionWithMetadata,missingOrNULL-method
setMethod("correlation", signature("CollectionWithMetadata", "missingOrNULL"), 
function(
  data1, 
  data2, 
  method = c('spearman','pearson'), 
  format  = c('ComputeResult', 'data.table'), 
  verbose = c(TRUE, FALSE), 
  proportionNonZeroThreshold = 0.05, 
  varianceThreshold = 0, 
  stdDevThreshold = 0, 
  metadataIsFirst = c(FALSE,TRUE)
) {
  
  format <- mbioUtils::matchArg(format)
  method <- mbioUtils::matchArg(method)
  verbose <- mbioUtils::matchArg(verbose)
  metadataIsFirst <- mbioUtils::matchArg(metadataIsFirst)
  
  #prefilters applied
  data1 <- pruneFeatures(data1, predicateFactory('proportionNonZero', proportionNonZeroThreshold), verbose)
  data1 <- pruneFeatures(data1, predicateFactory('variance', varianceThreshold), verbose)
  data1 <- pruneFeatures(data1, predicateFactory('sd', stdDevThreshold), verbose)
  
  values <- getCollectionData(data1, variableNames = NULL, ignoreImputeZero = FALSE, includeIds = FALSE, verbose = verbose)
  if (metadataIsFirst) {
    corrResult <- correlation(
      getSampleMetadata(data1, TRUE, FALSE), 
      values, 
      method = method, 
      format = 'data.table', 
      verbose = verbose
    )
  } else {
    corrResult <- correlation(
      values, 
      getSampleMetadata(data1, TRUE, FALSE), 
      method = method, 
      format = 'data.table', 
      verbose = verbose
    )
  }

  mbioUtils::logWithTime(
    paste(
      "Received df table with", 
      nrow(values), "samples and", 
      (ncol(values)-1), "features with values."
    ), 
    verbose
  )

  if (format == 'data.table') {
    return(corrResult)
  } else {
    if (metadataIsFirst) {
      result <- buildCorrelationComputeResult(corrResult, data1@sampleMetadata, data1, method, verbose)
    } else {
      result <- buildCorrelationComputeResult(corrResult, data1, data1@sampleMetadata, method, verbose)
    }
    result@computationDetails <- 'correlation'
    return(result)
  }
})

#' @rdname selfCorrelation
#' @aliases selfCorrelation,Collection-method
setMethod("selfCorrelation", signature("Collection"), 
function(
  data, 
  method = c('spearman','pearson'), 
  format = c('ComputeResult', 'data.table'), 
  verbose = c(TRUE, FALSE), 
  proportionNonZeroThreshold = 0.05, 
  varianceThreshold = 0, 
  stdDevThreshold = 0
) {
  
  format <- mbioUtils::matchArg(format)
  method <- mbioUtils::matchArg(method)
  verbose <- mbioUtils::matchArg(verbose)

  #prefilters applied
  data <- pruneFeatures(data, predicateFactory('proportionNonZero', proportionNonZeroThreshold), verbose)
  data <- pruneFeatures(data, predicateFactory('variance', varianceThreshold), verbose)
  data <- pruneFeatures(data, predicateFactory('sd', stdDevThreshold), verbose)

  values <- getCollectionData(data, variableNames = NULL, ignoreImputeZero = FALSE, includeIds = FALSE, verbose = verbose)
  corrResult <- correlation(values, NULL, method = method, format = 'data.table', verbose = verbose)

  mbioUtils::logWithTime(
    paste(
      "Received df table with", 
      nrow(values), "samples and", 
      (ncol(values)-1), "features with values."
    ), 
    verbose
  )

  if (format == 'data.table') {
    return(corrResult)
  } else {
    result <- buildCorrelationComputeResult(corrResult, data, NULL, method, verbose)
    result@computationDetails <- 'selfCorrelation'
    return(result)
  }  
})

#' @rdname selfCorrelation
#' @aliases selfCorrelation,SampleMetadata-method
setMethod("selfCorrelation", signature("SampleMetadata"), 
function(
  data, 
  method = c('spearman','pearson'), 
  format = c('ComputeResult', 'data.table'), 
  verbose = c(TRUE, FALSE)
) {
  format <- mbioUtils::matchArg(format)
  method <- mbioUtils::matchArg(method)
  verbose <- mbioUtils::matchArg(verbose)
  
  corrResult <- correlation(
    getSampleMetadata(data, TRUE, FALSE), 
    NULL, 
    method = method, 
    format = 'data.table', 
    verbose = verbose
  )

  mbioUtils::logWithTime(
    paste(
      "Received df table with", 
      nrow(data), "samples and", 
      (ncol(data)-1), "variables."
    ), 
    verbose
  )

  if (format == 'data.table') {
    return(corrResult)
  } else {
    result <- buildCorrelationComputeResult(corrResult, data, NULL, method, verbose)
    result@computationDetails <- 'selfCorrelation'
    return(result)
  }
})

#' @rdname correlation
#' @aliases correlation,Collection,Collection-method
setMethod("correlation", signature("Collection", "Collection"), 
function(
  data1, 
  data2, 
  method = c('spearman','pearson'), 
  format = c('ComputeResult', 'data.table'), 
  verbose = c(TRUE, FALSE), 
  proportionNonZeroThreshold = 0.05, 
  varianceThreshold = 0, 
  stdDevThreshold = 0
) {
  
  format <- mbioUtils::matchArg(format)
  method <- mbioUtils::matchArg(method)
  verbose <- mbioUtils::matchArg(verbose)
  
  #prefilters applied
  data1 <- pruneFeatures(data1, predicateFactory('proportionNonZero', proportionNonZeroThreshold), verbose)
  data1 <- pruneFeatures(data1, predicateFactory('variance', varianceThreshold), verbose)
  data1 <- pruneFeatures(data1, predicateFactory('sd', stdDevThreshold), verbose)
  data2 <- pruneFeatures(data2, predicateFactory('proportionNonZero', proportionNonZeroThreshold), verbose)
  data2 <- pruneFeatures(data2, predicateFactory('variance', varianceThreshold), verbose)
  data2 <- pruneFeatures(data2, predicateFactory('sd', stdDevThreshold), verbose)
  
  values1 <- getCollectionData(data1, variableNames = NULL, ignoreImputeZero = FALSE, includeIds = TRUE, verbose = verbose)
  values2 <- getCollectionData(data2, variableNames = NULL, ignoreImputeZero = FALSE, includeIds = TRUE, verbose = verbose)

  mbioUtils::logWithTime(
    paste(
      "Received first df table with", 
      nrow(values1), "samples and", 
      (ncol(values1)-1), "features with values."
    ), 
    verbose
  )
  mbioUtils::logWithTime(
    paste(
      "Received second df table with", 
      nrow(values2), "samples and", 
      (ncol(values2)-1), 
      "features with values."
    ), 
    verbose
  )

  # empty samples removed from data by getCollectionData, means we need to keep samples common to both datasets and remove ids
  # get id col names
  recordIdColumn <- data1@recordIdColumn
  allIdColumns <- c(recordIdColumn, data1@ancestorIdColumns)
  # should we verify that ids are the same in both datasets?

  data2IdColumns <- c(data2@recordIdColumn, data2@ancestorIdColumns)
  if (!all(allIdColumns %in% data2IdColumns)) {
    stop('All id columns from data1 must be present in data2')
  }

  # remove samples that are not common
  commonSamples <- intersect(values1[[recordIdColumn]], values2[[recordIdColumn]])
  if (length(commonSamples) == 0) {
    stop('No samples in common between data1 and data2')
  } else {
    mbioUtils::logWithTime(
      paste(
        "Found", length(commonSamples), "samples in common between data1 and data2. Only these samples will be used."
      ), 
      verbose
    )
  }

  values1 <- values1[values1[[recordIdColumn]] %in% commonSamples, ]
  values2 <- values2[values2[[recordIdColumn]] %in% commonSamples, ]

  # remove ids
  values1 <- values1[, -..allIdColumns]
  values2 <- values2[, -..allIdColumns]  

  corrResult <- mbioUtils::correlation(
    values1, 
    values2, 
    method = method, 
    format = 'data.table', 
    verbose = verbose
  )

  if (format == 'data.table') {
    return(corrResult)
  } else {
    result <- mbioUtils::buildCorrelationComputeResult(corrResult, data1, data2, method, verbose)
    result@computationDetails <- 'correlation'
    return(result)
  } 
})
