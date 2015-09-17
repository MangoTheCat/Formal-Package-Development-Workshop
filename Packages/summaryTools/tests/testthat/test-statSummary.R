context("Test the numeric component: statSummary")

test_that("correct summary for complete data", {

  # this object was generated using the dput function
  # as an example try dput(airquality$Ozone)
  sampleData <- c(7L, 40L, 16L, 43L, 10L, 77L, 41L, 6L, 20L, 98L, 50L, 99L, 7L, 
                   90L, 46L, 89L, 91L, 54L, 100L, 25L) 
  
  expect_equal(statSummary(sampleData), 
               structure(c(6, 50.45, 1205.73421052632, 100), .Names = c("Min", 
                                                                        "Mean", "Variance", "Max")))
  
})

