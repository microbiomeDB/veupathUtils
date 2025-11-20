check_collection <- function(object) {
    errors <- character()
    df <- object@data
    allIdColumns <- c(object@recordIdColumn, object@ancestorIdColumns)

    # check that name is not empty
    if (object@name == "") {
        msg <- "name cannot be empty"
        errors <- c(errors, msg)
    }

    # Ensure id columns are valid
    msg <- validateIdColumns(df, object@recordIdColumn, object@ancestorIdColumns)
    errors <- c(errors, msg)

    # check that all columns in data are numeric except id columns
    dataColNames <- names(df)[!names(df) %in% allIdColumns]
    if (!all(sapply(object@data[, ..dataColNames], is.numeric))) {
        msg <- sprintf("all columns in data except '%s' must be numeric", paste(allIdColumns, collapse=", "))
        errors <- c(errors, msg)
    }

    # collection data should all come from the same entity
    # using the presence of the period to indicate eda services formatted data
    if (all(grepl(".", names(df), fixed = TRUE))) {
        if (uniqueN(mbioUtils::strSplit(names(df)[!names(df) %in% object@ancestorIdColumns], ".", ncol=2, index=1)) > 1) {
            msg <- paste("All columns must belong to the same entity.")
            errors <- c(errors, msg)
        }
    }

    if (length(errors) == 0) {
        return(TRUE)
    } else {
        return(errors)
    }
}

#' Data Collection
#' 
#' This class represents a collection of related data. That could be
#' all of the abundance data for all samples in a dataset at a particular taxonomic rank,
#' or all pathway abundance data for all samples in a dataset, or something else. The
#' primary requirement for a collection is that the values of all variables in the
#' collection can be expressed on the same theoretical range.
#' @slot name The human-readable name of the collection. This can be anything that helps to identify the collection uniquely to the user.
#' @slot data A data.frame of data with samples as rows and variables as columns
#' @slot recordIdColumn The name of the column in the data.frame that contains the record id
#' @slot ancestorIdColumns A character vector of column names representing parent entities of the recordIdColumn
#' @slot imputeZero A logical indicating whether NA/ null values should be replaced with zeros.
#' @slot removeEmptyRecordss A logical indicating whether empty (all NA/ zero) records should be removed.
#' @name Collection-class
#' @rdname Collection-class
#' @export
setClass("Collection", 
    slots = c(
        name = "character",
        data = "data.table",
        recordIdColumn = "character",
        ancestorIdColumns = "character",
        imputeZero = 'logical',
        removeEmptyRecords = 'logical'
    ), prototype = prototype(
        imputeZero = TRUE,
        removeEmptyRecords = TRUE
    ),
    validity = check_collection
)


check_collections <- function(object) {
    errors <- character()

    # check that all names are unique
    if (length(unique(getCollectionNames(object))) != length(object)) {
        msg <- "collection names must be unique"
        errors <- c(errors, msg)
    }

    if (length(object) == 0) {
        return(TRUE)
    }

    # check that at least one ancestorIdColumn is shared between collections, or there are no ancestors
    firstCollectionAncestorIds <- object[[1]]@ancestorIdColumns
    if (!!length(firstCollectionAncestorIds)) {
        if (!all(sapply(object, function(x) any(x@ancestorIdColumns %in% firstCollectionAncestorIds)))) {
            msg <- "at least one ancestorIdColumn must be shared between collections"
            errors <- c(errors, msg)
        }
    } else {
        if (any(sapply(object, function(x) !!length(x@ancestorIdColumns)))) {
            msg <- "at least one ancestorIdColumn must be shared between collections"
            errors <- c(errors, msg)
        }
    }

    if (length(errors) == 0) {
        return(TRUE)
    } else {
        return(errors)
    }
}

#' Data Collections
#' 
#' This is a list of Data Collections.
#' @name Collections-class
#' @rdname Collections-class
#' @importFrom S4Vectors SimpleList
setClass("Collections", 
    contains = "SimpleList",
    prototype = prototype(
        elementType = "Collection"
    ),
    validity = check_collections
)

#' Create a Collection
#' 
#' This is a constructor for the Collection class. It creates a Collection
#' object for use in a Microbiome Dataset. 
#' @param name The human-readable name of the collection. This can be anything that helps to identify the collection uniquely to the user.
#' @param data A data.frame or a character vector representing a file path to a data.frame
#' @param recordIdColumn The name of the column in the data.frame that contains the record id
#' @param ancestorIdColumns A character vector of column names representing parent entities of the recordIdColumn
#' @export
#' @rdname Collection-methods
setGeneric("Collection", function(name, data, recordIdColumn, ancestorIdColumns) standardGeneric("Collection"))

#' @rdname Collection-methods
#' @aliases Collection,character,data.frame,character,character-method
setMethod("Collection", signature("character", "data.frame", "character", "character"), function(name, data, recordIdColumn, ancestorIdColumns) {
    data <- data.table::setDT(data)
    new("Collection", name = name, data = data, recordIdColumn = recordIdColumn, ancestorIdColumns = ancestorIdColumns)
})

#' @rdname Collection-methods
#' @aliases Collection,character,data.frame,missing,missing-method
setMethod("Collection", signature("character", "data.frame", "missing", "missing"), function(name, data, recordIdColumn, ancestorIdColumns) {
    warning("Id columns not specified, assuming first column is record id and other Id columns end in `_Id`")
    recordIdColumn <- findRecordIdColumn(names(data))
    ancestorIdColumns <- findAncestorIdColumns(names(data))
    data <- data.table::setDT(data)
    new("Collection", name = name, data = data, recordIdColumn = recordIdColumn, ancestorIdColumns = ancestorIdColumns)
})

#' @rdname Collection-methods
#' @aliases Collection,character,character,missing,missing-method
setMethod("Collection", signature("character", "character", "missing", "missing"), function(name, data, recordIdColumn, ancestorIdColumns) {
    data <- data.table::fread(data)
    warning("Id columns not specified, assuming first column is record id and other Id columns end in `_Id`")
    recordIdColumn <- findRecordIdColumn(names(data))
    ancestorIdColumns <- findAncestorIdColumns(names(data))
    new("Collection", name = name, data = data, recordIdColumn = recordIdColumn, ancestorIdColumns = ancestorIdColumns)
})

#' @rdname Collection-methods
#' @aliases Collection,character,character,character,character-method
setMethod("Collection", signature("character", "character", "character", "character"), function(name, data, recordIdColumn, ancestorIdColumns) {
    data <- data.table::fread(data)
    new("Collection", name =name, data = data, recordIdColumn = recordIdColumn, ancestorIdColumns = ancestorIdColumns)
})

#' @rdname Collection-methods
#' @aliases Collection,missing,missing,missing,missing-method
setMethod("Collection", signature("missing", "missing", "missing", "missing"), function(name, data, recordIdColumn, ancestorIdColumns) {
    new("Collection")
})


#' Create Collections
#' 
#' This is a constructor for the Collections class. It creates a Collections
#' object for use in a Microbiome Dataset. A Collections object is a list of
#' Collection objects.
#' @param collections A list of Collection objects, a data.frame containing multiple
#'  collections, or a character vector containing a file path to a data.frame
#' @param ontology A data.frame containing the ontology for the dataset
#' @return A Collections object
#' @export
#' @rdname Collections-methods
#' @include internal-utils.R
setGeneric("Collections", function(collections, ontology) standardGeneric("Collections"))

#' @rdname Collections-methods
#' @aliases Collections,missing,missing-method
setMethod("Collections", signature("missing", "missing"), function(collections, ontology) {
    new("Collections")
})

#' @rdname Collections-methods
#' @aliases Collections,list,missing-method
setMethod("Collections", signature("list", "missing"), function(collections, ontology) {
    if (length(collections) == 0) {
        new("Collections")
    } else {
        collectionsBuilder(collections)
    }
})

#' @rdname Collections-methods
#' @aliases Collections,list,data.frame-method
setMethod("Collections", signature("list", "data.frame"), function(collections, ontology) {
    if (length(collections) == 0) {
        new("Collections")
    } else {
        collectionsBuilder(collections, ontology)
    }
})

#' @rdname Collections-methods
#' @aliases Collections,data.frame,missing-method
setMethod("Collections", signature("data.frame", "missing"), function(collections, ontology) {
    if (nrow(collections) == 0) {
        new("Collections")
    } else {
        collectionsBuilder(list(collections))
    }  
})

#' @rdname Collections-methods
#' @aliases Collections,data.frame,data.frame-method
setMethod("Collections", signature("data.frame", "data.frame"), function(collections, ontology) {
    if (nrow(collections) == 0) {
        new("Collections")
    } else {
        collectionsBuilder(list(collections), ontology)
    }  
})

# For these two cases, the MbioDataset constructor should have already warned the user
# that ontology is not needed.
#' @rdname Collections-methods
#' @aliases Collections,Collection,missing-method
setMethod("Collections", signature("Collection", "missing"), function(collections, ontology) {
    new("Collections", list(collections))
})

#' @rdname Collections-methods
#' @aliases Collections,character,missing-method
setMethod("Collections", signature("character", "missing"), function(collections, ontology) {
    if (!length(collections) || collections == '') {
        new("Collections")
    } else {
        collectionsBuilder(list(collections))
    }
})