#' Get all ID columns
#' 
#' Returns a vector of all ID columns
#' 
#' @param object CollectionWithMetadata, or other object with slots recordIdColumn and ancestorIdColumns
#' @return vector of all ID columns
#' @rdname getIdColumns
#' @export
#' @include class-CollectionWithMetadata.R
setGeneric("getIdColumns",
    function(object) standardGeneric("getIdColumns"),
    signature = c("object")
)

#' @rdname getIdColumns
#' @aliases getIdColumns,ANY-method
setMethod("getIdColumns", "ANY", function(object) {
    if (all(c('recordIdColumn','ancestorIdColumns') %in% slotNames(object))) {
        return(c(object@recordIdColumn, object@ancestorIdColumns))
    } else {
        stop("Object does not have recordIdColumn and/or ancestorIdColumns slots. Received object of class ", class(object))
    }
})

#' Get Variable Names of Metadata
#' 
#' Get the names of the metadata variables in an object containing sample metadata.
#' @param object Any object with a slot containing sampleMetadata
#' @return a character vector of metadata variable names
#' @rdname getMetadataVariableNames
#' @export
setGeneric("getMetadataVariableNames", function(object) standardGeneric("getMetadataVariableNames"))

#' @rdname getMetadataVariableNames
#' @aliases getMetadataVariableNames,CollectionWithMetadata-method
setMethod("getMetadataVariableNames", "CollectionWithMetadata", function(object) return(names(object@sampleMetadata@data)))


#' Get Summary of Metadata Variables
#' 
#' Get a summary of the requested metadata variable in an object containing sample metadata.
#' @param object An object with a slot containing sampleMetadata
#' @param variable A character vector representing the name of the metadata variable to summarize
#' @return a table summarizing the values of the requested metadata variable
#' @rdname getMetadataVariableSummary
#' @export
setGeneric("getMetadataVariableSummary", function(object, variable) standardGeneric("getMetadataVariableSummary"), signature= c("object"))

#' @rdname getMetadataVariableSummary
#' @aliases getMetadataVariableSummary,CollectionWithMetadata-method
setMethod("getMetadataVariableSummary", "CollectionWithMetadata", function(object, variable) {
    if (!variable %in% getMetadataVariableNames(object)) {
        stop("Variable ", variable, " not found in sample metadata. Available variables: ", paste(getMetadataVariableNames(object), collapse = ", "))
    }

    varData <- object@sampleMetadata@data[[variable]]

    if (class(varData) %in% c('factor','character')) {
        out <- table(varData)
        dimnames(out) <- unname(dimnames(out))
        return(out)
    } else {
        return(summary(varData))
    }
})

#' Get Sample Metadata Id Column Names
#' 
#' Get the names of the record and ancestor id columns in the sample metadata of an object.
#' @param object An object w sample metadata
#' @return a character vector of id column names
#' @rdname getSampleMetadataIdColumns
#' @export
setGeneric("getSampleMetadataIdColumns", function(object) standardGeneric("getSampleMetadataIdColumns"))

#' @rdname getSampleMetadataIdColumns
#' @aliases getSampleMetadataIdColumns,CollectionWithMetadata-method
setMethod("getSampleMetadataIdColumns", "CollectionWithMetadata", function(object) getIdColumns(object@sampleMetadata))

#' Get data.table of sample metadata from CollectionWithMetadata
#'
#' Returns a data.table of sample metadata
#' 
#' @param object CollectionWithMetadata
#' @param asCopy boolean indicating whether to return the data as a copy or by reference
#' @param includeIds boolean indicating whether we should include recordIdColumn and ancestorIdColumns
#' @param metadataVariables The metadata variables to include in the sample metadata. If NULL, all metadata variables will be included.
#' @return data.table of sample metadata
#' @import mbioUtils
#' @import data.table
#' @rdname getSampleMetadata
#' @export
setGeneric("getSampleMetadata",
    function(object, asCopy = c(TRUE, FALSE), includeIds = c(TRUE, FALSE), metadataVariables = NULL) standardGeneric("getSampleMetadata"),
    signature = c("object")
)

#' @rdname getSampleMetadata
#' @aliases getSampleMetadata,CollectionWithMetadata-method
setMethod("getSampleMetadata", signature("CollectionWithMetadata"), function(object, asCopy = c(TRUE, FALSE), includeIds = c(TRUE, FALSE), metadataVariables = NULL) {
    asCopy <- mbioUtils::matchArg(asCopy)
    includeIds <- mbioUtils::matchArg(includeIds)

    dt <- object@sampleMetadata@data
    allIdColumns <- getSampleMetadataIdColumns(object)

    # Check that incoming dt meets requirements
    if (!inherits(dt, 'data.table')) {
        data.table::setDT(dt)
    }

    if (asCopy) {
        dt <- data.table::copy(dt)
    }

    if (object@removeEmptyRecords) {
        # not using getCollectionData here bc i want the empty records here
        abundances <- object@data[, -..allIdColumns]

        # Remove metadata for records with NA or 0 in all columns
        dt <- dt[rowSums(isNAorZero(abundances)) != ncol(abundances),]
    }

    if (includeIds && !is.null(metadataVariables)) {
        dt <- dt[, c(allIdColumns, metadataVariables), with = FALSE]
    } else if (!includeIds && !is.null(metadataVariables)) {
        dt <- dt[, metadataVariables, with = FALSE]
    } else if (!includeIds && is.null(metadataVariables)) {
        dt <- dt[, -..allIdColumns]
    }

    return(dt)
})

#' Drop Records with incomplete SampleMetadata
#'
#' Modifies the data and sampleMetadata slots of an 
#' CollectionWithMetadata object, to exclude records with 
#' missing SampleMetadata for a specified column.
#' 
#' @param object CollectionWithMetadata
#' @param colName String providing the column name in SampleMetadata to check for completeness
#' @param verbose boolean indicating if timed logging is desired
#' @return CollectionWithMetadata with modified data and sampleMetadata slots
#' @rdname removeIncompleteRecords
#' @export
setGeneric("removeIncompleteRecords",
    function(object, colName = character(), verbose = c(TRUE, FALSE)) standardGeneric("removeIncompleteRecords"),
    signature = c("object")
)

#' @rdname removeIncompleteRecords
#' @aliases removeIncompleteRecords,CollectionWithMetadata-method
setMethod("removeIncompleteRecords", signature("CollectionWithMetadata"), function(object, colName = character(), verbose = c(TRUE, FALSE)) {
    verbose <- mbioUtils::matchArg(verbose)
    df <- getCollectionData(object, verbose = verbose)
    sampleMetadata <- getSampleMetadata(object)
    # df may have had rows removed due to getCollectionData behavior. Subset sampleMetadata to match
    sampleMetadata <- sampleMetadata[sampleMetadata[[object@sampleMetadata@recordIdColumn]] %in% df[[object@recordIdColumn]], ]

    # Remove Records with NA from data and metadata
    if (any(is.na(sampleMetadata[[colName]]))) {
        mbioUtils::logWithTime("Found NAs in specified variable. Removing these records.", verbose)
        recordsWithData <- which(!is.na(sampleMetadata[[colName]]))
        # Keep records with data. Recall the CollectionWithMetadata object requires records to be in the same order
        # in both the data and metadata
        sampleMetadata <- sampleMetadata[recordsWithData, ]
        df <- df[recordsWithData, ]

        object@data <- df
        object@sampleMetadata <- SampleMetadata(
            data = sampleMetadata,
            recordIdColumn = object@sampleMetadata@recordIdColumn
        )
        
        validObject(object)
    }

    return(object)
})