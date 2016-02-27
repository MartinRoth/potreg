library(data.table)
library(potreg)

if (!file.exists("./testsInput/testData.rds")) {
  admNL <- raster::getData('GADM', country='NL', path="./testsInput", level=0)
  testData <- eobsR::importEOBS('rr', '2010/2014', admNL, "0.50reg")
  testData <- testData[pointID == 1]
  testData <- testData[ !(year == 2011 & month == 4), ]
  testData <- testData[ !(year == 2011 & month == 7), ]
  testData <- testData[ !(year == 2011 & month == 9), ]
  saveRDS(testData, file="./testsInput/testData.rds")
} else testData <- readRDS("./testsInput/testData.rds")

context("Input testing:")
expect_equal_to_reference(testData, file="./testsOutput/inputData.rds")


context("Output testing:")
expect_equal_to_reference(qCondPoisProcess(0.025, 1 : 65, 65, 235), file="./testsOutput/lowerLimitPoissonProcess.rds")
expect_equal_to_reference(qCondPoisProcess(0.975, 1 : 65, 65, 235), file="./testsOutput/upperLimitPoissonProcess.rds")
expect_equal_to_reference(SubsetSeasons(testData, "winter"), file="./testsOutput/inputDataWinters.rds")
expect_equal_to_reference(SubsetSeasons(testData, "spring"), file="./testsOutput/inputDataSprings.rds")
expect_equal_to_reference(SubsetSeasons(testData, "summer"), file="./testsOutput/inputDataSummers.rds")
expect_equal_to_reference(SubsetSeasons(testData, "autumn"), file="./testsOutput/inputDataAutumns.rds")
expect_error(fitGpdFlex(dat), "This should be tested.")

