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


context("Input data testing:")
expect_equal_to_reference(testData, file="./testsOutput/inputData.rds")


context("Output testing:")
expect_equal_to_reference(qCondPoisProcess(0.025, 1 : 65, 65, 235), file="./testsOutput/lowerLimitPoissonProcess.rds")
expect_equal_to_reference(qCondPoisProcess(0.975, 1 : 65, 65, 235), file="./testsOutput/upperLimitPoissonProcess.rds")
expect_equal_to_reference(SubsetSeasons(testData, "winter"), file="./testsOutput/winters.rds")
expect_equal_to_reference(SubsetSeasons(testData, "spring"), file="./testsOutput/springs.rds")
expect_equal_to_reference(SubsetSeasons(testData, "summer"), file="./testsOutput/summers.rds")
expect_equal_to_reference(SubsetSeasons(testData, "autumn"), file="./testsOutput/autumns.rds")

context("Test fitGpdFlex:")
if(!file.exists("../../inst/extdata/NL_eobs_threshold_0.50.rds")) {
  print("fitGpdFlex will be skipped")
} else {
  data <- readRDS("../../inst/extdata/NL_eobs_threshold_0.50.rds")
  data[, pVal := NULL]
  data[, excess := rrSep - threshold]
  data[excess <= 0, excess := NA]
  setkey(data, pointID, time)
  xpar <- list()
  xpar$S <- data[, length(unique(pointID))]
  xpar$T <- data[pointID == 1, .N]
  xpar$julian <- data[pointID == 1, as.numeric(julian(time, origin=as.Date("1950-01-01")))]
  xpar$threshold <- data[, threshold]
  dim(xpar$threshold) <- c(xpar$T, xpar$S)
  datMat <- data[, excess]
  dim(datMat) <- c(xpar$T, xpar$S)
  modelA <- function(p, xpar) {
    scale <- p[1] * xpar$threshold
    shape <- matrix(p[2], xpar$T, xpar$S)
    list(scale = scale, shape = shape)
  }

  startA <- c(0.5, 0.0)
  expect_equal_to_reference(data,   file="./testsInput/inputFitGpd.rds")
  expect_equal_to_reference(xpar,   file="./testsInput/inputXpar.rds")
  expect_equal_to_reference(datMat, file="./testsInput/inputDatMat.rds")
  expect_equal_to_reference(modelA, file="./testsInput/inputFpar.rds")
  expect_equal_to_reference(startA, file="./testsInput/inputStartValues.rds")
  expect_equal_to_reference(fitGpdFlex(data = datMat, xpar = xpar, fpar = modelA, hessian = TRUE,
             numberOfParameters = 2, start = startA,  method = "BFGS",
             control = list(maxit = 5000, ndeps = c(1e-03, 1e-05))),
             file="./testsOutput/fitGpdObject.rds")
}

