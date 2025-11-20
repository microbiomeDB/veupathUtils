test_that('ComputeResult validation works', {
  
  nSamples <- 200
  df <- data.table::data.table(
    'entity.SampleID' = paste0('sample_', 1:nSamples),
    'entity.contA' = rnorm(nSamples)
  )
  names(df) <- stripEntityIdFromColumnHeader(names(df))
  df$alphaDiversity <- .1

  computedVariableMetadata <- mbioUtils::VariableMetadata(
                 variableClass = mbioUtils::VariableClass(value = "computed"),
                 variableSpec = mbioUtils::VariableSpec(variableId = 'alphaDiversity', entityId = 'entity'),
                 plotReference = mbioUtils::PlotReference(value = "yAxis"),
                 displayName = "Alpha Diversity",
                 displayRangeMin = 0,
                 displayRangeMax = max(max(df$alphaDiversity, na.rm = TRUE),1),
                 dataType = mbioUtils::DataType(value = "NUMBER"),
                 dataShape = mbioUtils::DataShape(value = "CONTINUOUS")
   )

  expect_error(new("ComputeResult",
                recordIdColumn = 'entity.SampleID',
                computedVariableMetadata = mbioUtils::VariableMetadataList(S4Vectors::SimpleList(computedVariableMetadata)),
                data = df))
  expect_error(new("ComputeResult",
                name = c('alphaDiv', 'test'),
                recordIdColumn = 'entity.SampleID',
                computedVariableMetadata = mbioUtils::VariableMetadataList(S4Vectors::SimpleList(computedVariableMetadata)),
                data = df))
  expect_error(new("ComputeResult",
                name = NULL,
                recordIdColumn = 'entity.SampleID',
                computedVariableMetadata = NULL,
                data = df))
  expect_error(new("ComputeResult",
                name = NA,
                recordIdColumn = 'entity.SampleID',
                computedVariableMetadata = NULL,
                data = df))
  expect_error(new("ComputeResult",
                name = 'alphaDiv',
                recordIdColumn = 'entity.SampleID',
                computedVariableMetadata = NULL,
                data = df))
  expect_error(new("ComputeResult",
                name = 'alphaDiv',
                recordIdColumn = 'entity.SampleID',
                data = df))
  expect_error(new("ComputeResult",
                name = 'alphaDiv',
                recordIdColumn = 'entity.SampleID',
                computedVariableMetadata = NA,
                data = df))

  df$alphaDiversity <- NULL
  expect_error(new("ComputeResult",
                name = c('alphaDiv'),
                recordIdColumn = 'entity.SampleID',
                computedVariableMetadata = mbioUtils::VariableMetadataList(S4Vectors::SimpleList(computedVariableMetadata)),
                data = df))

  df$alphaDiversity <- .1
  computedVariableMetadata <- mbioUtils::VariableMetadata(
                 variableClass = mbioUtils::VariableClass(value = "native"),
                 variableSpec = mbioUtils::VariableSpec(variableId = 'alphaDiversity', entityId = 'entity'),
                 plotReference = mbioUtils::PlotReference(value = "yAxis"),
                 displayName = "Alpha Diversity",
                 displayRangeMin = 0,
                 displayRangeMax = max(max(df$alphaDiversity, na.rm = TRUE),1),
                 dataType = mbioUtils::DataType(value = "NUMBER"),
                 dataShape = mbioUtils::DataShape(value = "CONTINUOUS")
   )

   expect_error(new("ComputeResult",
                name = c('alphaDiv'),
                recordIdColumn = 'entity.SampleID',
                computedVariableMetadata = mbioUtils::VariableMetadataList(S4Vectors::SimpleList(computedVariableMetadata)),
                data = df))

  # TODO test that extra cols in data will err, dot notation of output cols or not, column order
})

test_that("ComputeResult writeMeta method returns well formatted json", {
  nSamples <- 200
  df <- data.table::data.table(
    'entity.SampleID' = paste0('sample_', 1:nSamples),
    'entity.contA' = rnorm(nSamples)
  )
  names(df) <- stripEntityIdFromColumnHeader(names(df))
  df$alphaDiversity <- .1

  computedVariableMetadata <- mbioUtils::VariableMetadata(
                 variableClass = mbioUtils::VariableClass(value = "computed"),
                 variableSpec = mbioUtils::VariableSpec(variableId = 'alphaDiversity', entityId = 'entity'),
                 plotReference = mbioUtils::PlotReference(value = "yAxis"),
                 displayName = "Alpha Diversity",
                 displayRangeMin = 0,
                 displayRangeMax = max(max(df$alphaDiversity, na.rm = TRUE),1),
                 dataType = mbioUtils::DataType(value = "NUMBER"),
                 dataShape = mbioUtils::DataShape(value = "CONTINUOUS")
   )

  result <- new("ComputeResult",
                name = "alphaDiv",
                recordIdColumn = 'entity.SampleID',
                computedVariableMetadata = mbioUtils::VariableMetadataList(S4Vectors::SimpleList(computedVariableMetadata)),
                data = df)

  jsonlist <- jsonlite::fromJSON(mbioUtils::toJSON(result@computedVariableMetadata))

  expect_equal(names(jsonlist), 'variables')
  expect_equal(names(jsonlist$variables), c("variableClass","variableSpec","plotReference","displayName","displayRangeMin","displayRangeMax","dataType","dataShape","isCollection","imputeZero","hasStudyDependentVocabulary"))
})


test_that("ComputeResult writeStatistics returns formatted json.", {

  nStats <- 100
  nSamples <- 200
  df <- data.table::data.table(
    'entity.SampleID' = paste0('sample_', 1:nSamples),
    'entity.contA' = rnorm(nSamples)
  )
  names(df) <- stripEntityIdFromColumnHeader(names(df))
  result <- new("ComputeResult",
              name = "differentialAbundance",
              recordIdColumn = 'entity.SampleID',
              statistics = data.frame(list("stat_float" = rnorm(nStats, sd=5),
                                           "stat_char" = sample(letters,nStats,replace=T),
                                           "stat_with_missing" = sample(c(1.2, 3, NA), nStats, replace=T)
                                           )),
              data = df)

  # transform to character, just like in the real method
  charObject <- data.frame(lapply(result@statistics, as.character))
  outJson <- jsonlite::toJSON(charObject)
  jsonList <- jsonlite::fromJSON(outJson)
  
  expect_equal(length(jsonList$stat_float), nStats) # Check we still have enough stats
  expect_equal(names(jsonList), c('stat_float','stat_char','stat_with_missing'))
  expect_equal(unname(unlist(lapply(jsonList,class))), c('character','character','character'))
  expect_equal(jsonList$stat_with_missing, as.character(result@statistics$stat_with_missing))
  expect_equal(jsonList$stat_float, as.character(result@statistics$stat_float))

})
