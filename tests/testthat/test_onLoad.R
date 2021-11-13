testthat::context(".onLoad")

testthat::test_that(".onLoad works",{
  D2MCS:::.onLoad()
  options <- D2MCSOptions$new()
  op <- options$getAll()
  testthat::expect_equal(d2mcs.Options$getAll(), op)
  testthat::expect_equal(class(getOption("loggerSettings")$logger[[1]]),
                         "Logger")
})
