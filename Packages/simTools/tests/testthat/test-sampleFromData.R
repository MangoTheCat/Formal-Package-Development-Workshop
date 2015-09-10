context("sampleFromData must return data frames of the correct format")


test_that("Default arguments return correctly", {

  set.seed(20)
  testData <- sampleFromData(airquality, 3)

  expect_is(testData, "data.frame")
  expect_named(testData, c("Ozone", "Solar.R", "Wind", "Temp", "Month", "Day"))
  expect_equal(testData[,"Month"], c(9, 8, 6))
  expect_equal(testData[,"Day"], c(12, 26, 12))
})

test_that("Throws errors",{

  expect_error(sampleFromData(airquality, "Subject"),
               "Size must be a numeric integer value")
})
