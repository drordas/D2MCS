testthat::setup({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})

testthat::test_that("d2mcs.log verbose option not set",{

  message <- "exampleMessage"
  level <- "INFO"
  className <- "exampleClassName"
  methodName <- "exampleMethodName"

  d2mcs.Options$remove("verbose")

  testthat::expect_error(d2mcs.log(message = message,
                                   level = level,
                                   className = className,
                                   methodName = methodName),
                         "[d2mcs.log][FATAL] Verbose is not defined in d2mcs.Options",
                         fixed = TRUE)
})

testthat::teardown({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})

testthat::setup({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})

testthat::test_that("d2mcs.log logger not configured",{

  message <- "exampleMessage"
  level <- "INFO"
  className <- "exampleClassName"
  methodName <- "exampleMethodName"

  d2mcs.Options$disableLog()

  testthat::expect_silent(d2mcs.log(message = message,
                                    level = level,
                                    className = className,
                                    methodName = methodName))
})

testthat::teardown({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})

testthat::setup({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})

testthat::test_that("d2mcs.log logger NULL",{

  message <- "exampleMessage"
  level <- "INFO"
  className <- "exampleClassName"
  methodName <- "exampleMethodName"

  d2mcs.Options$set("verbose", TRUE)
  D2MCS:::.setLoggerSettings(settings = NULL)

  testthat::expect_error(d2mcs.log(message = message,
                                   level = level,
                                   className = className,
                                   methodName = methodName),
                         "[d2mcs.log][FATAL] Logger is not configured. Use d2mcs.options$configureLog to configure its behavior",
                         fixed = TRUE)
})

testthat::teardown({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})

testthat::setup({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})

testthat::test_that("d2mcs.log error level parameter",{

  message <- "exampleMessage"
  className <- "exampleClassName"
  methodName <- "exampleMethodName"

  level <- NULL

  testthat::expect_error(d2mcs.log(message = message,
                                   level = level,
                                   className = className,
                                   methodName = methodName),
                         "[d2mcs.log][FATAL] The 'level' variable must be between these values: FATAL, ERROR, WARN, INFO or DEBUG",
                         fixed = TRUE)

  level <- 1

  testthat::expect_error(d2mcs.log(message = message,
                                   level = level,
                                   className = className,
                                   methodName = methodName),
                         "[d2mcs.log][FATAL] The 'level' variable must be between these values: FATAL, ERROR, WARN, INFO or DEBUG",
                         fixed = TRUE)

  level <- "wrongLevel"

  testthat::expect_error(d2mcs.log(message = message,
                                   level = level,
                                   className = className,
                                   methodName = methodName),
                         "[d2mcs.log][FATAL] The 'level' variable must be between these values: FATAL, ERROR, WARN, INFO or DEBUG",
                         fixed = TRUE)

})

testthat::teardown({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})

testthat::setup({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog(threshold = "DEBUG")

})

testthat::test_that("d2mcs.log works",{

  message <- "exampleMessage"

  className <- "exampleClassName"
  methodName <- "exampleMethodName"

  level <- "DEBUG"
  d2mcs.Options$set("verbose", TRUE)
  testthat::expect_message(d2mcs.log(message = message,
                                     level = level,
                                     className = className,
                                     methodName = methodName),
                           "[exampleClassName][exampleMethodName][DEBUG] exampleMessage",
                           fixed = TRUE)

  level <- "INFO"

  testthat::expect_message(d2mcs.log(message = message,
                                     level = level,
                                     className = className,
                                     methodName = methodName),
                           "[exampleClassName][exampleMethodName][INFO] exampleMessage",
                           fixed = TRUE)

  level <- "WARN"

  testthat::expect_warning(d2mcs.log(message = message,
                                     level = level,
                                     className = className,
                                     methodName = methodName),
                           "[exampleClassName][exampleMethodName][WARN] exampleMessage",
                           fixed = TRUE)

  level <- "ERROR"

  testthat::expect_message(d2mcs.log(message = message,
                                     level = level,
                                     className = className,
                                     methodName = methodName),
                           "[exampleClassName][exampleMethodName][ERROR] exampleMessage",
                           fixed = TRUE)

  level <- "FATAL"

  testthat::expect_error(d2mcs.log(message = message,
                                   level = level,
                                   className = className,
                                   methodName = methodName),
                         "[exampleClassName][exampleMethodName][FATAL] exampleMessage",
                         fixed = TRUE)

})

testthat::teardown({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})

testthat::setup({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})

testthat::test_that("d2mcs.log works with FALTAL|ERROR levels when loggers are disable",{

  message <- "exampleMessage"
  level <- "FATAL"
  className <- "exampleClassName"
  methodName <- "exampleMethodName"

  d2mcs.Options$disableLog()

  testthat::expect_error(d2mcs.log(message = message,
                                   level = level,
                                   className = className,
                                   methodName = methodName),
                         "[exampleClassName][exampleMethodName][FATAL] exampleMessage",
                         fixed = TRUE)

  level <- "WARN"

  testthat::expect_warning(d2mcs.log(message = message,
                                     level = level,
                                     className = className,
                                     methodName = methodName),
                           "[exampleClassName][exampleMethodName][WARN] exampleMessage",
                           fixed = TRUE)
})

testthat::teardown({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})
