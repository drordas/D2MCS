testthat::test_that("NoProbability: initialize", {

  testthat::expect_is(NoProbability$new(),
                      "NoProbability")
})
