testthat::setup({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})

testthat::test_that("PPV: initialize function works", {

  lvs <- c("normal", "abnormal")
  truth <- factor(rep(lvs, times = c(86, 258)),
                  levels = rev(lvs))
  pred <- factor(
    c(
      rep(lvs, times = c(54, 32)),
      rep(lvs, times = c(27, 231))),
    levels = rev(lvs))

  xtab <- table(pred, truth)

  confMatrix <- ConfMatrix$new(confMatrix = caret::confusionMatrix(xtab))

  testthat::expect_is(PPV$new(performance = confMatrix),
                      "PPV")
})

testthat::teardown({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})

testthat::setup({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})

testthat::test_that("PPV: compute function works", {

  lvs <- c("normal", "abnormal")
  truth <- factor(rep(lvs, times = c(86, 258)),
                  levels = rev(lvs))
  pred <- factor(
    c(
      rep(lvs, times = c(54, 32)),
      rep(lvs, times = c(27, 231))),
    levels = rev(lvs))

  xtab <- table(pred, truth)

  confMatrix <- ConfMatrix$new(confMatrix = caret::confusionMatrix(xtab))

  testthat::expect_is(PPV$new(performance = confMatrix)$compute(performance.output = NULL),
                      "numeric")

  testthat::expect_is(PPV$new(performance = NULL)$compute(performance.output = confMatrix),
                      "numeric")
})

testthat::teardown({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})

testthat::setup({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})

testthat::test_that("PPV: compute function checks parameter type", {
  testthat::expect_error(PPV$new(performance = NULL)$compute(performance.output = NULL),
                         "[PPV][compute][FATAL] Performance output parameter must be defined as 'MinResult' or 'ConfMatrix' type. Aborting...",
                         fixed = TRUE)
})

testthat::setup({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})
