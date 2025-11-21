# TODO roxygen documentation
# TODO consider renaming to VariableMapping to be more in line w EdaCommon

variable_classes <- c('native', 'derived', 'computed')
#the other option is to just let this be any character vector so long as it has only a single value..
plot_references <- c('xAxis', 'yAxis', 'zAxis', 'overlay', 'facet1', 'facet2', 'geo', 'latitude', 'longitude')
data_types <- c('NUMBER', 'STRING', 'INTEGER', 'DATE', 'LONGITUDE')
data_shapes <- c('CONTINUOUS', 'CATEGORICAL', 'ORDINAL', 'BINARY')

# these are annoying bc im essentially doing this to enforce an enum type in R
# maybe see if theres a better way
check_variable_class <- function(object) {
    errors <- character()
    variable_class <- object@value
    
    if (length(variable_class) != 1) {
      msg <- "Variable class must have a single value."
      errors <- c(errors, msg) 
    }

    if (suppressWarnings(!variable_class %in% variable_classes)) {
      msg <- paste("Variable class must be one of", paste(variable_classes, collapse = ", "))
      errors <- c(errors, msg)
    }

    return(if (length(errors) == 0) TRUE else errors)
}

check_plot_reference <- function(object) {
    errors <- character()
    plot_reference <- object@value
    
    if (length(plot_reference) != 1) {
      msg <- "Plot reference must have a single value."
      errors <- c(errors, msg) 
    }

    if (suppressWarnings(!plot_reference %in% plot_references)) {
      msg <- paste("Plot reference must be one of", paste(plot_references, collapse = ", "))
      errors <- c(errors, msg)
    }

    return(if (length(errors) == 0) TRUE else errors)
}

check_data_type <- function(object) {
    errors <- character()
    data_type <- object@value
    
    if (length(data_type) != 1) {
      msg <- "Data type must have a single value."
      errors <- c(errors, msg) 
    }

    if (suppressWarnings(!data_type %in% data_types)) {
      msg <- paste("Data type must be one of", paste(data_types, collapse = ", "))
      errors <- c(errors, msg)
    }

    return(if (length(errors) == 0) TRUE else errors)
}

check_data_shape <- function(object) {
    errors <- character()
    data_shape <- object@value

    if (length(data_shape) != 1) {
      msg <- "Data shape must have a single value."
      errors <- c(errors, msg) 
    }

    if (suppressWarnings(!data_shape %in% data_shapes)) {
      msg <- paste("Data shape must be one of", paste(data_shapes, collapse = ", "))
      errors <- c(errors, msg)
    }

    return(if (length(errors) == 0) TRUE else errors)
}

#' @export
VariableClass <- setClass("VariableClass", representation(
    value = 'character'
), prototype = prototype(
    value = NA_character_
), validity = check_variable_class)

#' Variable Specification
#' 
#' A class for working with variable specifications. Contains a variable ID and an entity ID.
#' @slot variableId The name of the variable
#' @slot entityId The name of the entity
#' @name VariableSpec-class
#' @rdname VariableSpec-class
#' @export
VariableSpec <- setClass("VariableSpec", representation(
    variableId = "character",
    entityId = "character"
), prototype = prototype(
    variableId = NA_character_,
    entityId = NA_character_
))

#' @export
PlotReference <- setClass("PlotReference", representation(
  value = 'character'
), prototype = prototype(
  value = NA_character_
), validity = check_plot_reference)

#' @importFrom S4Vectors SimpleList
#' @export
VariableSpecList <- setClass("VariableSpecList", 
  contains = "SimpleList", 
  prototype = prototype(elementType = "VariableSpec")
)

#' @export
DataType <- setClass("DataType", representation(
    value = 'character'
), prototype = prototype(
    value = NA_character_
), validity = check_data_type)

#' @export
DataShape <- setClass("DataShape", representation(
    value = 'character'
), prototype = prototype(
    value = NA_character_
), validity = check_data_shape)

check_variable_metadata <- function(object) {
    data_type <- object@dataType@value
    min <- object@displayRangeMin
    max <- object@displayRangeMax
    variable_spec <- object@variableSpec
    variable_class <- object@variableClass@value
    plot_reference <- object@plotReference@value

    errors <- character()
    # class, plotRef, varId and entityId must be non-empty
    # TODO not sure this check is working
    if (!length(variable_class)) errors <- c(errors, "Variable class must be non-empty.")
    
    if (!length(variable_spec)) {
      errors <- c(errors, "VariableSpec must be non-empty.")
    } else {
        if (!length(variable_spec@variableId)) errors <- c(errors, "Variable Id must be non-empty.")
        if (!length(variable_spec@entityId)) errors <- c(errors, "Entity Id must be non-empty.")
    }

    if (!length(plot_reference)) errors <- c(errors, "Plot reference must be non-empty.")

    # need display ranges, vocab etc for derived and computed vars
    if (any(c('derived', 'computed') %in% variable_class)) {
      if (is.na(object@displayName)) errors <- c(errors, "Display name must be non-empty for derived or computed variables.")

      # display range min/max must be numeric for numeric types, string for dates, NULL else
      if (is.na(min) && data_type != "STRING") {
        errors <- c(errors, "Display range min must be non-empty for derived or computed variables.")
      } else {
        if (data_type == "NUMBER" && !is.numeric(min)) errors <- c(errors, "Display range min must be numeric for data type 'NUMBER'.")
        if (data_type == "DATE" && !is.character(min)) errors <- c(errors, "Display range min must be of type character for data type 'DATE'.")
        if (data_type == "STRING" && !is.na(min)) errors <- c(errors, "Display range min must be NA for data type 'STRING'")
      }

      if (is.na(max) && data_type != "STRING") {
        errors <- c(errors, "Display range max must be non-empty for derived or computed variables.")
      } else {
        if (data_type == "NUMBER" && !is.numeric(max)) errors <- c(errors, "Display range max must be numeric for data type 'NUMBER'.")
        if (data_type == "DATE" && !is.character(max)) errors <- c(errors, "Display range max must be of type character for data type 'DATE'.")
        if (data_type == "STRING" && !is.na(max)) errors <- c(errors, "Display range max must be NA for data type 'STRING'")
      }

    }

    # need members only for collections
    if (object@isCollection) {
      if (!length(object@members)) {
        errors <- c(errors, "Members must be non-empty for collection variables.")
      } else {
        memberEntityIds <- unlist(lapply(as.list(object@members), function(x) {return(x@entityId)}))
        memberColNames <- unlist(lapply(as.list(object@members), function(x) {return(mbioUtils::getColName(x))}))
        
        # Require all members to have the same entity
        if (data.table::uniqueN(memberEntityIds) > 1) {
          errors <- c(errors, "All members in a collection must have the same entity id.")
        }

        # Ensure no two variables are the same
        if (any(duplicated(memberColNames))) {
          errors <- c(errors, "All members in a collection must be unique.")
        }
      }
    }

    return(if (length(errors) == 0) TRUE else errors)
}

#' @export
VariableMetadata <- setClass("VariableMetadata", representation(
    variableClass = 'VariableClass',
    variableSpec = 'VariableSpec',
    plotReference = 'PlotReference',
    displayName = 'character',
    displayRangeMin = 'ANY',
    displayRangeMax = 'ANY',
    dataType = 'DataType',
    dataShape = 'DataShape',
    vocabulary = 'character',
    isCollection = 'logical',
    imputeZero = 'logical',
    weightingVariableSpec = 'VariableSpec',
    hasStudyDependentVocabulary = 'logical',
    members = 'VariableSpecList'
), prototype = prototype(
    displayName = NA_character_,
    displayRangeMin = NA_real_,
    displayRangeMax = NA_real_,
    vocabulary = NA_character_,
    isCollection = FALSE,
    imputeZero = FALSE,
    hasStudyDependentVocabulary = FALSE
), validity = check_variable_metadata)

#' @export
VariableMetadataList <- setClass("VariableMetadataList",
  contains = "SimpleList",
  prototype = prototype(elementType = "VariableMetadata")
)
