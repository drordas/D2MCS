testthat::test_that("BinaryPlot: plot function works", {

  plot <- BinaryPlot$new()

  summary <- data.frame(c(1, 2), c(2, 3))
  names(summary) <- c("k", "dispersion")

  testthat::expect_equal(c("gg", "ggplot"), class(plot$plot(summary)))
})

testthat::test_that("BinaryPlot: plot function checks parameter type", {

  plot <- BinaryPlot$new()

  testthat::expect_error(plot$plot("wrong"),
                         "[BinaryPlot][FATAL] Summary parameter must be defined as 'data.frame' type. Aborting...",
                         fixed = TRUE)
})
