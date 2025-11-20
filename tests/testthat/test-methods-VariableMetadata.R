test_that("merge returns sane results", {
    collectionVM <- new("VariableMetadata",
                         variableClass = new("VariableClass", value = c("native")),
                         variableSpec = new("VariableSpec", variableId = 'abc', entityId = 'def'),
                         displayName = 'Im a collection variable',
                         dataType = new("DataType", value = 'NUMBER'),
                         dataShape = new("DataShape", value = 'CONTINUOUS'),
                         isCollection = TRUE,
                         members = new("VariableSpecList", S4Vectors::SimpleList(
                           new("VariableSpec", variableId = 'a', entityId = 'b'),
                           new("VariableSpec", variableId = 'c', entityId = 'b')
                         ))
                       )
    cvmlist <- new("VariableMetadataList", S4Vectors::SimpleList(collectionVM))

    vm <- new("VariableMetadata",
                 variableClass = new("VariableClass", value = "native"),
                 variableSpec = new("VariableSpec", variableId = 'abc', entityId = 'def'),
                 dataType = new("DataType", value = 'NUMBER'),
                 dataShape = new("DataShape", value = 'CONTINUOUS')
            )
    vmlist <- new("VariableMetadataList", S4Vectors::SimpleList(vm))

    expect_equal(merge(cvmlist, vmlist), new("VariableMetadataList", S4Vectors::SimpleList(collectionVM, vm)))
    expect_equal(merge(cvmlist, merge(vmlist, cvmlist)), new("VariableMetadataList", S4Vectors::SimpleList(collectionVM, vm, collectionVM)))
})

test_that("getColName returns column names or NULL", {
  variableSpec = new("VariableSpec", variableId = 'abc', entityId = 'def')
  expect_equal(getColName(variableSpec), 'def.abc')

  variableSpec = new("VariableSpec", variableId = '', entityId = 'def')
  expect_error(getColName(variableSpec))

  variableSpec = new("VariableSpec", variableId = 'abc', entityId = '')
  expect_equal(getColName(variableSpec), 'abc')

  variableSpec = new("VariableSpec", variableId = NA_character_, entityId = 'def')
  expect_error(getColName(variableSpec))

  variableSpec = new("VariableSpec", variableId = 'abc', entityId = NA_character_)
  expect_equal(getColName(variableSpec), 'abc')

  expect_equal(getColName(NULL), NULL)
})

#findCollectionVariableMetadata
test_that("findCollectionVariableMetadata returns sane results", {
    collectionVM <- new("VariableMetadata",
                         variableClass = new("VariableClass", value = c("native")),
                         variableSpec = new("VariableSpec", variableId = 'abc', entityId = 'def'),
                         displayName = 'Im a collection variable',
                         dataType = new("DataType", value = 'NUMBER'),
                         dataShape = new("DataShape", value = 'CONTINUOUS'),
                         isCollection = TRUE,
                         members = new("VariableSpecList", S4Vectors::SimpleList(
                           new("VariableSpec", variableId = 'a', entityId = 'b'),
                           new("VariableSpec", variableId = 'c', entityId = 'b')
                         ))
                       )

    vm <- new("VariableMetadata",
                 variableClass = new("VariableClass", value = "native"),
                 variableSpec = new("VariableSpec", variableId = 'abc', entityId = 'def'),
                 dataType = new("DataType", value = 'NUMBER'),
                 dataShape = new("DataShape", value = 'CONTINUOUS')
            )

    expect_equal(findCollectionVariableMetadata(new("VariableMetadataList", SimpleList(collectionVM, vm))), collectionVM)
    expect_equal(findCollectionVariableMetadata(new("VariableMetadataList", SimpleList(vm, collectionVM))), collectionVM)
    expect_equal(findCollectionVariableMetadata(new("VariableMetadataList")), NULL)
    expect_equal(findCollectionVariableMetadata(new("VariableMetadataList", SimpleList(vm, vm))), NULL)
    expect_error(findCollectionVariableMetadata(new("VariableSpecList")))    
})

#findIndexFromPlotRef -- what if there is no match?
test_that("findIndexFromPlotRef returns sane results", {
    collectionVM <- new("VariableMetadata",
                         variableClass = new("VariableClass", value = c("native")),
                         variableSpec = new("VariableSpec", variableId = 'abc', entityId = 'def'),
                         plotReference = new("PlotReference", value='xAxis'),
                         displayName = 'Im a collection variable',
                         dataType = new("DataType", value = 'NUMBER'),
                         dataShape = new("DataShape", value = 'CONTINUOUS'),
                         isCollection = TRUE,
                         members = new("VariableSpecList", S4Vectors::SimpleList(
                           new("VariableSpec", variableId = 'a', entityId = 'b'),
                           new("VariableSpec", variableId = 'c', entityId = 'b')
                         ))
                       )

    vm <- new("VariableMetadata",
                 variableClass = new("VariableClass", value = "native"),
                 variableSpec = new("VariableSpec", variableId = 'abc', entityId = 'def'),
                 plotReference = new("PlotReference", value = 'facet1'),
                 dataType = new("DataType", value = 'NUMBER'),
                 dataShape = new("DataShape", value = 'CONTINUOUS')
            )

    expect_equal(findIndexFromPlotRef(new("VariableMetadataList", SimpleList(collectionVM, vm)), 'xAxis'), 1)
    expect_equal(findIndexFromPlotRef(new("VariableMetadataList", SimpleList(vm, collectionVM)), 'xAxis'), 2)
    expect_equal(findIndexFromPlotRef(new("VariableMetadataList", SimpleList(collectionVM, vm)), 'facet1'), 2)
    expect_equal(findIndexFromPlotRef(new("VariableMetadataList", SimpleList(vm, collectionVM)), 'facet1'), 1)
    expect_equal(findIndexFromPlotRef(new("VariableMetadataList")), NULL)
    expect_equal(findIndexFromPlotRef(new("VariableMetadataList", SimpleList(vm, vm)), 'xAxis'), NULL)
    expect_error(findIndexFromPlotRef(new("VariableSpecList")))    
})

#findColNamesFromPlotRef -- collections too
test_that("findColsNamesFromPlotRef returns sane results", {
    collectionVM <- new("VariableMetadata",
                         variableClass = new("VariableClass", value = c("native")),
                         variableSpec = new("VariableSpec", variableId = 'abc', entityId = 'def'),
                         plotReference = new("PlotReference", value = 'xAxis'),
                         displayName = 'Im a collection variable',
                         dataType = new("DataType", value = 'NUMBER'),
                         dataShape = new("DataShape", value = 'CONTINUOUS'),
                         isCollection = TRUE,
                         members = new("VariableSpecList", S4Vectors::SimpleList(
                           new("VariableSpec", variableId = 'a', entityId = 'b'),
                           new("VariableSpec", variableId = 'c', entityId = 'b')
                         ))
                       )

    vm <- new("VariableMetadata",
                 variableClass = new("VariableClass", value = "native"),
                 variableSpec = new("VariableSpec", variableId = 'abc', entityId = 'def'),
                 plotReference = new("PlotReference", value = 'facet1'),
                 dataType = new("DataType", value = 'NUMBER'),
                 dataShape = new("DataShape", value = 'CONTINUOUS')
            )

    expect_equal(findColNamesFromPlotRef(new("VariableMetadataList", SimpleList(collectionVM, vm)), 'xAxis'), c('b.a', 'b.c'))
    expect_equal(findColNamesFromPlotRef(new("VariableMetadataList", SimpleList(vm, collectionVM)), 'xAxis'), c('b.a', 'b.c'))
    expect_equal(findColNamesFromPlotRef(new("VariableMetadataList", SimpleList(collectionVM, vm)), 'facet1'), 'def.abc')
    expect_equal(findColNamesFromPlotRef(new("VariableMetadataList", SimpleList(vm, collectionVM)), 'facet1'), 'def.abc')
    expect_equal(findColNamesFromPlotRef(new("VariableMetadataList")), NULL)
    expect_equal(findColNamesFromPlotRef(new("VariableMetadataList", SimpleList(vm, vm)), 'overlay'), NULL)
    expect_error(findColNamesFromPlotRef(new("VariableSpecList")))    
})

#findDataTypesFromPlotRef -- prob dont need both type and shape
test_that("findDataTypesFromPlotRef returns sane results", {
    collectionVM <- new("VariableMetadata",
                         variableClass = new("VariableClass", value = c("native")),
                         variableSpec = new("VariableSpec", variableId = 'abc', entityId = 'def'),
                         plotReference = new("PlotReference", value = 'xAxis'),
                         displayName = 'Im a collection variable',
                         dataType = new("DataType", value = 'NUMBER'),
                         dataShape = new("DataShape", value = 'CONTINUOUS'),
                         isCollection = TRUE,
                         members = new("VariableSpecList", S4Vectors::SimpleList(
                           new("VariableSpec", variableId = 'a', entityId = 'b'),
                           new("VariableSpec", variableId = 'c', entityId = 'b')
                         ))
                       )

    vm <- new("VariableMetadata",
                 variableClass = new("VariableClass", value = "native"),
                 variableSpec = new("VariableSpec", variableId = 'abc', entityId = 'def'),
                 plotReference = new("PlotReference", value = 'facet1'),
                 dataType = new("DataType", value = 'DATE'),
                 dataShape = new("DataShape", value = 'CONTINUOUS')
            )

    expect_equal(findDataTypesFromPlotRef(new("VariableMetadataList", SimpleList(collectionVM, vm)), 'xAxis'), 'NUMBER')
    expect_equal(findDataTypesFromPlotRef(new("VariableMetadataList", SimpleList(vm, collectionVM)), 'xAxis'), 'NUMBER')
    expect_equal(findDataTypesFromPlotRef(new("VariableMetadataList", SimpleList(collectionVM, vm)), 'facet1'), 'DATE')
    expect_equal(findDataTypesFromPlotRef(new("VariableMetadataList", SimpleList(vm, collectionVM)), 'facet1'), 'DATE')
    expect_equal(findDataTypesFromPlotRef(new("VariableMetadataList")), NULL)
    expect_equal(findDataTypesFromPlotRef(new("VariableMetadataList", SimpleList(vm, vm)), 'xAxis'), NULL)
    expect_error(findDataTypesFromPlotRef(new("VariableSpecList")))    
})

test_that("toJSON result is properly formatted for VariableMetadata", {
    vm <- new("VariableMetadata",
                 variableClass = new("VariableClass", value = c("computed")),
                 variableSpec = new("VariableSpec", variableId = 'abc', entityId = 'def'),
                 plotReference = new("PlotReference", value = 'xAxis'),
                 displayName = 'Im a computed variable',
                 displayRangeMin = 1,
                 displayRangeMax = 10,
                 dataType = new("DataType", value = 'NUMBER'),
                 dataShape = new("DataShape", value = 'CONTINUOUS'),
                 isCollection = TRUE,
                 imputeZero = TRUE,
                 members = new("VariableSpecList", S4Vectors::SimpleList(
                    new("VariableSpec", variableId = 'a', entityId = 'b'),
                    new("VariableSpec", variableId = 'c', entityId = 'b')
                 ))
            )
    vmjson <- mbioUtils::toJSON(vm)
    vmlist <- jsonlite::fromJSON(vmjson)

    expect_equal(names(vmlist), 'variableMetadata')
    expect_equal(names(vmlist$variableMetadata), c('variableClass', 'variableSpec', 'plotReference', 'displayName', 'displayRangeMin', 'displayRangeMax', 'dataType', 'dataShape', 'isCollection', 'imputeZero', 'hasStudyDependentVocabulary', 'members'))
    expect_equal(vmlist$variableMetadata$variableClass, c('computed'))
    expect_equal(names(vmlist$variableMetadata$variableSpec), c('variableId', 'entityId'))
    expect_equal(class(vmlist$variableMetadata$displayRangeMin), 'character')
    expect_equal(class(vmlist$variableMetadata$displayRangeMin), 'character')
    expect_equal(is.character(vmlist$variableMetadata$dataType), TRUE)
    expect_equal(is.character(vmlist$variableMetadata$dataShape), TRUE)
    expect_equal(vmlist$variableMetadata$imputeZero, TRUE)
    expect_equal(vmlist$variableMetadata$isCollection, TRUE)
    expect_equal(length(vmlist$variableMetadata$members), 2)
    #this becomes data frame w 2 rows
    expect_equal(names(vmlist$variableMetadata$members), c('variableId', 'entityId'))

    vm <- new("VariableMetadata",
                 variableClass = new("VariableClass", value = c("computed")),
                 variableSpec = new("VariableSpec", variableId = 'abc', entityId = 'def'),
                 displayName = 'Im a computed variable',
                 dataType = new("DataType", value = 'STRING'),
                 dataShape = new("DataShape", value = 'BINARY'),
                 vocabulary = c('a', 'b')
            )
    vmjson <- mbioUtils::toJSON(vm)
    vmlist <- jsonlite::fromJSON(vmjson)

    expect_equal(names(vmlist), 'variableMetadata')
    # no plotReference
    expect_equal(names(vmlist$variableMetadata), c('variableClass', 'variableSpec', 'displayName', 'dataType', 'dataShape', 'vocabulary', 'isCollection', 'imputeZero', 'hasStudyDependentVocabulary'))
    expect_equal(vmlist$variableMetadata$variableClass, c('computed'))
    expect_equal(names(vmlist$variableMetadata$variableSpec), c('variableId', 'entityId'))
    expect_equal(is.character(vmlist$variableMetadata$dataType), TRUE)
    expect_equal(is.character(vmlist$variableMetadata$dataShape), TRUE)
    expect_equal(vmlist$variableMetadata$isCollection, FALSE)
    expect_equal(vmlist$variableMetadata$isCollection, FALSE)
    expect_equal(length(vmlist$variableMetadata$vocabulary), 2)
})