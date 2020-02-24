testthat::test_that("TwoClass: initialize", {
  testthat::expect_is(TwoClass$new(method = "cv", number = 10, savePredictions = "final",
                                    classProbs = TRUE, allowParallel = TRUE, verboseIter = FALSE),
                      "TwoClass")
})
