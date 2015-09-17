context("Test the whole summary function: numericSummary")

test_that("The correct summayr is returned",  {

  sampleData <- c(7, 40, 16, 43, 10, 77, 41, 6, 20, 98, 50, 99, 7, 
                  90, 46, 89, 91, 54, 100, 25)   
  
  testOutput <- numericSummary(sampleData)
  
  expect_named(testOutput, c("Min", "Mean", "Variance", "Max", "NAs"))
  
  expect_equal(testOutput, structure(c(6, 50.45, 1205.73421052632, 100, 0), .Names = c("Min", 
                                                                                       "Mean", "Variance", "Max", "NAs")))
  
})

test_that("Missing values are correctly handled", {
  
  testData <- c(13, NA, 35, 28, 7, NA, 29, 34, 42, 8) 
  
  testOutput <- numericSummary(testData)
  
  expect_equal(testOutput["NAs"], c(NAs = 2))
  
})

test_that("Errors are correctly handled", {

  expect_error(numericSummary("Hello!"), "provide a numeric vector")  
  
  expect_error(numericSummary(1:10, na.rm = 5), "na.rm must be a logical")
  
})


test_that("Correct warnings are given", {

  expect_warning(numericSummary(1), "summaries may be incorrect")  
  
})

