#####      some helpers for building Collections objects from various data sources       #####
##### these are not intended to be used outside of this package or the mbio main package #####

#' @export
findCollectionId <- function(dataColName) {
    # this is the case where the id columns follow no format and the data columns follow `Name [CollectionId_VariableId]` format (downloads)
    if (grepl("\\[", dataColName)) {
        varId <- strsplit(dataColName, "\\[")[[1]][2]
        collectionId <- regmatches(varId,regexpr("^([^_]*_[^_]*)",varId))
        return(collectionId)
    }

    # this presumably the case where the column headers follow the `entityId.variableId` format (the eda services format)
    if (grepl(".", dataColName, fixed = TRUE)) {
        varId <- strsplit(dataColName, ".", fixed = TRUE)[[1]][1]
        collectionId <- strsplit(varId, "_", fixed = TRUE)[[1]][1]
        return(collectionId)
    }

    stop((sprintf("Could not find collection id for column: %s. Unrecognized format.", dataColName)))
}

#' @export
findCollectionIds <- function(dataColNames) {
    recordIdColumn <- findRecordIdColumn(dataColNames)
    ancestorIdColumns <- findAncestorIdColumns(dataColNames)
    variableColNames <- dataColNames[!dataColNames %in% c(recordIdColumn,ancestorIdColumns)]

    return(unique(unlist(sapply(variableColNames, findCollectionId))))
}

#' @export
findRecordIdColumn <- function(dataColNames) {
    # for now assume were working w bulk download files, which means its the first column
    allIdColumns <- dataColNames[grepl("_Id$", dataColNames)]
    return(allIdColumns[1])
}

#' @export
findAncestorIdColumns <- function(dataColNames) {
    # for now assume were working w bulk download files, which means they have '_Id'
    allIdColumns <- dataColNames[grepl("_Id$", dataColNames)]
    if (length(allIdColumns) == 1) {
        return(character(0))
    }

    return(allIdColumns[2:length(allIdColumns)])
}

#' @export
clean_names <- function(names, makeUnique = FALSE) {
    # remove everything after the last opening square bracket to get rid of IRIs
    names <- gsub("\\[.*$", "", names)

    names <- gsub("%+", "_pct_", names)
    names <- gsub("\\$+", "_dollars_", names)
    names <- gsub("\\++", "_plus_", names)
    names <- gsub("-+", "_minus_", names)
    names <- gsub("\\*+", "_star_", names)
    names <- gsub("#+", "_cnt_", names)
    names <- gsub("&+", "_and_", names)
    names <- gsub("@+", "_at_", names)

    names <- gsub("[^a-zA-Z0-9_]+", "_", names)
    names <- gsub("([A-Z][a-z])", "_\\1", names)
    names <- tolower(trimws(names))

    names <- gsub("(^_+|_+$)", "", names)

    names <- gsub("_+", "_", names)

    if (makeUnique) names <- make.unique(names, sep = "_")

    return(names)
}

## this thing will take the name of a file or a data.frame and return a cleaned data.table
## its intended to be used for reading in variable collections from the EDA full-dataset download files
## keepIdsAndNumbersOnly will ignore things like presence/absence of a bug on the assay entities
## cleanColumnNames will clean up the column names to make them valid column names in R, and hopefully improve consistncy of labels as well
#' @export
getDataFromSource <- function(dataSource, keepIdsAndNumbersOnly = c(TRUE, FALSE), cleanColumnNames = c(FALSE, TRUE)) {
    keepIdsAndNumbersOnly <- mbioUtils::matchArg(keepIdsAndNumbersOnly)
    cleanColumnNames <- mbioUtils::matchArg(cleanColumnNames)

    if (inherits(dataSource, "character")) {
        mbioUtils::logWithTime(sprintf("Attempting to read file: %s", dataSource), verbose = TRUE)
        dt <- data.table::fread(dataSource, na.strings=c(''))
    } else if (inherits(dataSource, "data.frame")) {
        dt <- data.table::as.data.table(dataSource)        
    }

    dataColNames <- names(dt)
    recordIdColumn <- findRecordIdColumn(dataColNames)
    ancestorIdColumns <- findAncestorIdColumns(dataColNames)

    # keep only rows that have values for id cols
    dt <- dt[!is.na(dt[[recordIdColumn]])]

    # theres probably a better way to do this..
    # the idea is that some assay entities have things like presence/ absence of a bug. 
    # they show up as character columns w values like 'Y' and 'N', but were not supporting these data for now.
    if (keepIdsAndNumbersOnly) {
        numericColumns <- dataColNames[which(sapply(dt,is.numeric))]
        dt <- dt[, unique(c(recordIdColumn, ancestorIdColumns, numericColumns)), with=FALSE]
    }

    if (cleanColumnNames) {
        names(dt)[!names(dt) %in% c(recordIdColumn, ancestorIdColumns)] <- clean_names(names(dt)[!names(dt) %in% c(recordIdColumn, ancestorIdColumns)])
    }

    return(dt)
}

#' @export
findCollectionDataColumns <- function(dataColNames, collectionId) {
    return(dataColNames[grepl(collectionId, dataColNames, fixed=TRUE)])
}

#' @export
getCollectionName <- function(collectionId, dataSourceName, ontology = NULL) {
    if (grepl("16S", dataSourceName, fixed=TRUE)) {
        dataSourceName <- paste("16S", regmatches(dataSourceName,regexpr("(\\(.*?)\\)",dataSourceName)))
    }

    if (grepl("Metagenomic", dataSourceName, fixed=TRUE)) {
        dataSourceName <- "Shotgun metagenomics"
    }

    if (grepl("Mass_spectrometry", dataSourceName, fixed=TRUE)) {
        dataSourceName <- "Metabolomics"
    }

    if (!is.null(ontology)) {
        # this assumes were getting one of our own ontology download files
        # w columns like `iri` and `label`
        collectionLabel <- unique(ontology$label[ontology$iri == collectionId])
    	if (collectionLabel %in% c('Kingdom','Phylum','Class','Order','Family','Genus','Species')) {
            collectionLabel <- unique(paste(collectionLabel, paste0("(", ontology$parentlabel[ontology$iri == collectionId], ")")))
	    }

        if (length(collectionLabel) == 1) {
            return(paste(dataSourceName, collectionLabel))
        } else {
            warning("Could not find collection label for collection id: ", collectionId)
        }
    }

    return(paste(dataSourceName, collectionId, sep=": "))
}

# so i considered that these should be constructors or something maybe.. 
# but i mean them to only ever be used internally so im not going to worry about it until something forces me to
#' @export
collectionBuilder <- function(collectionId, dt, ontology = NULL) {
    dataColNames <- names(dt)
    collectionColumns <- findCollectionDataColumns(dataColNames, collectionId)
    recordIdColumn <- findRecordIdColumn(dataColNames)
    ancestorIdColumns <- findAncestorIdColumns(dataColNames)

    collection <- new("Collection", 
        name=getCollectionName(collectionId, recordIdColumn, ontology),
        data=dt[, c(recordIdColumn, ancestorIdColumns, collectionColumns), with = FALSE],
        recordIdColumn=recordIdColumn,
        ancestorIdColumns=ancestorIdColumns
    )

    return(collection)
}

#' @export
getCollectionsList <- function(dataSource, ontology = NULL) {
    if (inherits(dataSource, "Collection")) return(dataSource)

    dt <- getDataFromSource(dataSource)
    dataColNames <- names(dt)
    collectionIds <- findCollectionIds(dataColNames)

    collections <- lapply(collectionIds, collectionBuilder, dt, ontology)

    return(collections)
}

#' @export
collectionsBuilder <- function(dataSources, ontology = NULL) {
    collectionsLists <- lapply(dataSources, getCollectionsList, ontology)
    collections <- unlist(collectionsLists, recursive = FALSE)

    collections <- new("Collections", collections)

    return(collections)
}
