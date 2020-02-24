testthat::test_that("MaximizeFN: initialize", {

  testthat::expect_is(MaximizeFN$new(required.metrics = c("MCC", "PPV")),
                      "MaximizeFN")
})

testthat::test_that("MaximizeFN: initialize checks parameter type", {

  testthat::expect_error(MaximizeFN$new(required.metrics = NULL),
                         "[MaximizeFN][FATAL] Invalid values of required.metrics. Aborting...",
                         fixed = TRUE)
})

testthat::test_that("MaximizeFN: getFinalPrediction function works", {

  maxFN <- MaximizeFN$new(required.metrics = c("MCC", "PPV"))

  raw.pred <- list("Positive", "Negative")
  names(raw.pred) <- c("MCC", "PPV")
  prob.pred <- list(0.6, 0.5)
  names(prob.pred) <- c("MCC", "PPV")
  positive.class <- "Positive"
  negative.class <- "Negative"

  testthat::expect_true(maxFN$getFinalPrediction(raw.pred = raw.pred,
                                                 prob.pred = prob.pred,
                                                 positive.class = positive.class,
                                                 negative.class = negative.class))

  raw.pred <- list("Negative", "Negative")
  names(raw.pred) <- c("MCC", "PPV")

  testthat::expect_false(maxFN$getFinalPrediction(raw.pred = raw.pred,
                                                  prob.pred = prob.pred,
                                                  positive.class = positive.class,
                                                  negative.class = negative.class))
})

testthat::test_that("MaximizeFN: getFinalPrediction function checks parameter type", {

  maxFN <- MaximizeFN$new(required.metrics = c("MCC", "PPV"))

  raw.pred <- NULL
  prob.pred <- list(0.6, 0.5)
  names(prob.pred) <- c("MCC", "PPV")
  positive.class <- "Positive"
  negative.class <- "Negative"

  testthat::expect_error(maxFN$getFinalPrediction(raw.pred = raw.pred,
                                                  prob.pred = prob.pred,
                                                  positive.class = positive.class,
                                                  negative.class = negative.class),
                         "[MaximizeFN][FATAL] Raw.pred parameter must be defined as 'list' type. Aborting...",
                         fixed = TRUE)

  raw.pred <- list("Positive", "Positive")
  names(raw.pred) <- c("MCC", "xxx")
  prob.pred <- list(0.6, 0.5)
  names(prob.pred) <- c("MCC", "PPV")
  positive.class <- "Positive"
  negative.class <- "Negative"

  testthat::expect_error(maxFN$getFinalPrediction(raw.pred = raw.pred,
                                                  prob.pred = prob.pred,
                                                  positive.class = positive.class,
                                                  negative.class = negative.class),
                         "[MaximizeFN][FATAL] Raw.pred parameter must have required metrics. MCC PPV. Aborting...",
                         fixed = TRUE)

  raw.pred <- list("Positive", "Positive")
  names(raw.pred) <- c("MCC", "PPV")
  prob.pred <- NULL
  positive.class <- "Positive"
  negative.class <- "Negative"

  testthat::expect_error(maxFN$getFinalPrediction(raw.pred = raw.pred,
                                                  prob.pred = prob.pred,
                                                  positive.class = positive.class,
                                                  negative.class = negative.class),
                         "[MaximizeFN][FATAL] Prob.pred parameter must be defined as 'list' type. Aborting...",
                         fixed = TRUE)

  raw.pred <- list("Positive", "Positive")
  names(raw.pred) <- c("MCC", "PPV")
  prob.pred <- list(0.6, 0.5)
  names(prob.pred) <- c("MCC", "xxx")
  positive.class <- "Positive"
  negative.class <- "Negative"

  testthat::expect_error(maxFN$getFinalPrediction(raw.pred = raw.pred,
                                                  prob.pred = prob.pred,
                                                  positive.class = positive.class,
                                                  negative.class = negative.class),
                         "[MaximizeFN][FATAL] Prob.pred parameter must have required metrics. MCC PPV. Aborting...",
                         fixed = TRUE)

  raw.pred <- list("Positive", "Positive")
  names(raw.pred) <- c("MCC", "PPV")
  prob.pred <- list(0.6, 0.5)
  names(prob.pred) <- c("MCC", "PPV")
  positive.class <- NULL
  negative.class <- "Negative"

  testthat::expect_error(maxFN$getFinalPrediction(raw.pred = raw.pred,
                                                  prob.pred = prob.pred,
                                                  positive.class = positive.class,
                                                  negative.class = negative.class),
                         "[MaximizeFN][FATAL] Positive class parameter must be defined as 'character' type. Aborting...",
                         fixed = TRUE)

  raw.pred <- list("Positive", "Positive")
  names(raw.pred) <- c("MCC", "PPV")
  prob.pred <- list(0.6, 0.5)
  names(prob.pred) <- c("MCC", "PPV")
  positive.class <- "Positive"
  negative.class <- NULL

  testthat::expect_error(maxFN$getFinalPrediction(raw.pred = raw.pred,
                                                  prob.pred = prob.pred,
                                                  positive.class = positive.class,
                                                  negative.class = negative.class),
                         "[MaximizeFN][FATAL] Negative class parameter must be defined as 'character' type. Aborting...",
                         fixed = TRUE)

})
