testthat::test_that("initialize",{
  testthat::expect_silent(D2MCSOptions$new())
})

testthat::test_that("get",{
  options <- D2MCSOptions$new()
  testthat::expect_true(options$get("verbose"))
})

testthat::test_that("get key type error",{
  options <- D2MCSOptions$new()
  testthat::expect_error(options$get(1),
                         "[D2MCSOptions][get][FATAL] Checking the type of the 'key' variable: numeric",
                         fixed = TRUE)
  testthat::expect_error(options$get("a"),
                         "[D2MCSOptions][get][FATAL] 'a' option is not configured",
                         fixed = TRUE)

})

testthat::test_that("add",{
  options <- D2MCSOptions$new()
  testthat::expect_invisible(options$add("new", 2))
  testthat::expect_equal(options$get("new"), 2)
})

testthat::test_that("add key type error",{
  options <- D2MCSOptions$new()
  testthat::expect_error(options$add(1, 1),
                         "[D2MCSOptions][add][FATAL] Checking the type of the 'key' variable: numeric",
                         fixed = TRUE)
  testthat::expect_error(options$add("verbose", 2),
                         "[D2MCSOptions][add][FATAL] 'verbose' option is already configured with the value: ",
                         fixed = TRUE)

})

testthat::test_that("set",{
  options <- D2MCSOptions$new()
  testthat::expect_invisible(options$set("verbose", FALSE))
  testthat::expect_equal(options$get("verbose"), FALSE)

  testthat::expect_invisible(options$set("verbose", NULL))
  testthat::expect_null(options$get("verbose"))
})

testthat::test_that("set key type error",{
  options <- D2MCSOptions$new()
  testthat::expect_error(options$set(1, 1),
                         "[D2MCSOptions][set][FATAL] Checking the type of the 'key' variable: numeric",
                         fixed = TRUE)
  testthat::expect_error(options$set("a", 2),
                         "[D2MCSOptions][set][FATAL] 'a' option is not configured",
                         fixed = TRUE)
})

testthat::test_that("remove",{
  options <- D2MCSOptions$new()
  testthat::expect_invisible(options$remove("verbose"))
  testthat::expect_error(options$get("verbose"))
})

testthat::test_that("remove key type error",{
  options <- D2MCSOptions$new()
  testthat::expect_error(options$remove(1),
                         "[D2MCSOptions][remove][FATAL] Checking the type of the 'key' variable: numeric",
                         fixed = TRUE)
  testthat::expect_error(options$remove("a"),
                         "[D2MCSOptions][remove][FATAL] 'a' option is not configured",
                         fixed = TRUE)

})

testthat::test_that("getAll",{
  options <- D2MCSOptions$new()
  testthat::expect_type(options$getAll(),"list")
})

testthat::test_that("reset",{
  options <- D2MCSOptions$new()

  op <- options$getAll()
  options$add("new", 2)
  testthat::expect_invisible(options$reset())
  testthat::expect_equal(options$getAll(), op)
})

testthat::test_that("isSpecificOption",{
  options <- D2MCSOptions$new()
  testthat::expect_true(options$isSpecificOption("verbose"))
  testthat::expect_false(options$isSpecificOption("a"))
})

testthat::test_that("print",{
  options <- D2MCSOptions$new()
  testthat::expect_output(print(options), "[A-Za-z0-9$/.]+")
})

testthat::setup({
  d2mcs.Options$configureLog()
  if (!dir.exists(normalizePath(path = file.path(tempdir(),
                                                 "testFiles",
                                                 "testD2MCSOptions"),
                                winslash = "/",
                                mustWork = FALSE))) {

    dir.create(path = normalizePath(path = file.path(tempdir(),
                                                     "testFiles",
                                                     "testD2MCSOptions"),
                                    winslash = "/",
                                    mustWork = FALSE),
               recursive = TRUE)
  }
})

testthat::test_that("configureLog works",{

  options <- D2MCSOptions$new()

  console <- TRUE
  threshold <- "INFO"
  log.file <- file.path(tempdir(),
                        "testFiles",
                        "testD2MCSOptions",
                        "log.txt")

  options$configureLog(console = console,
                       threshold = threshold,
                       file = log.file)

  testthat::expect_message(options$getLogConfiguration(),
                           "[D2MCSOptions][getLogConfiguration][INFO] Log configuration:
	- Threshold: INFO
	- Console log status: Actived
	- File log status: Actived",
                           fixed = TRUE)
})

testthat::teardown({
  d2mcs.Options$configureLog()
  if (dir.exists(normalizePath(path = file.path(tempdir(),
                                                "testFiles",
                                                "testD2MCSOptions"),
                               winslash = "/",
                               mustWork = FALSE))) {
    log.file <- file.path(tempdir(),
                          "testFiles",
                          "testD2MCSOptions")
    unlink(log.file, recursive = TRUE, force = TRUE)
  }
})

testthat::setup({
  d2mcs.Options$configureLog()
  if (!dir.exists(normalizePath(path = file.path(tempdir(),
                                                 "testFiles",
                                                 "testD2MCSOptions"),
                                winslash = "/",
                                mustWork = FALSE))) {

    dir.create(path = normalizePath(path = file.path(tempdir(),
                                                     "testFiles",
                                                     "testD2MCSOptions"),
                                    winslash = "/",
                                    mustWork = FALSE),
               recursive = TRUE)
  }
})

testthat::test_that("configureLog mode file works",{

  options <- D2MCSOptions$new()

  console <- TRUE
  threshold <- "INFO"
  log.file <- file.path(tempdir(),
                        "testFiles",
                        "testD2MCSOptions",
                        "log.txt")

  options$configureLog(console = console,
                       threshold = threshold,
                       file = log.file)

  d2mcs.Options$configureLog(console = console,
                             threshold = threshold,
                             file = log.file)

  message <- "message"
  level <- "INFO"
  className <- "className"
  methodName <- "methodName"

  d2mcs.log(message, level, className, methodName)

  testthat::expect_true(file.exists(log.file))

  testthat::expect_match(readLines(log.file),
                         "[-\\[\\]:0-9 ]+\\[className\\]\\[methodName\\]\\[INFO\\] message",
                         perl = TRUE)
})

testthat::teardown({
  d2mcs.Options$configureLog()
  if (dir.exists(normalizePath(path = file.path(tempdir(),
                                                "testFiles",
                                                "testD2MCSOptions"),
                               winslash = "/",
                               mustWork = FALSE))) {
    log.file <- file.path(tempdir(),
                          "testFiles",
                          "testD2MCSOptions")
    unlink(log.file, recursive = TRUE, force = TRUE)
  }
})

testthat::test_that("configureLog error threshold argument",{

  options <- D2MCSOptions$new()

  console <- TRUE
  file <- NULL

  threshold <- NULL

  testthat::expect_error(options$configureLog(console = console,
                                              threshold = threshold,
                                              file = file),
                         "[D2MCSOptions][configureLog][FATAL] The 'threshold' parameter must be between these values: FATAL, ERROR, WARN, INFO or DEBUG",
                         fixed = TRUE)

  threshold <- 1

  testthat::expect_error(options$configureLog(console = console,
                                              threshold = threshold,
                                              file = file),
                         "[D2MCSOptions][configureLog][FATAL] The 'threshold' parameter must be between these values: FATAL, ERROR, WARN, INFO or DEBUG",
                         fixed = TRUE)

  threshold <- "wrongThreshold"

  testthat::expect_error(options$configureLog(console = console,
                                              threshold = threshold,
                                              file = file),
                         "[D2MCSOptions][configureLog][FATAL] The 'threshold' parameter must be between these values: FATAL, ERROR, WARN, INFO or DEBUG",
                         fixed = TRUE)
})

testthat::test_that("configureLog error console argument",{

  options <- D2MCSOptions$new()

  console <- NULL
  threshold <- "INFO"
  file <- NULL

  testthat::expect_error(options$configureLog(console = console,
                                              threshold = threshold,
                                              file = file),
                         "[D2MCSOptions][configureLog][FATAL] Checking the type of the 'console' variable: NULL",
                         fixed = TRUE)
})

testthat::setup({
  d2mcs.Options$configureLog()
})

testthat::test_that("disableLog works",{

  options <- D2MCSOptions$new()

  options$disableLog()

  testthat::expect_message(options$getLogConfiguration(),
                           "[D2MCSOptions][getLogConfiguration][INFO] Log configuration:
	- Threshold: INFO
	- Console log status: Disabled
	- File log status: Disabled",
                           fixed = TRUE)
})

testthat::teardown({
  d2mcs.Options$configureLog()
})

testthat::setup({
  d2mcs.Options$configureLog()
})

testthat::test_that("getLogConfiguration works",{

  options <- D2MCSOptions$new()

  testthat::expect_message(options$getLogConfiguration(),
                           "[D2MCSOptions][getLogConfiguration][INFO] Log configuration:
	- Threshold: INFO
	- Console log status: Actived
	- File log status: Disabled",
                           fixed = TRUE)

  console <- FALSE
  threshold <- "INFO"
  file <- NULL

  options$configureLog(console = console,
                       threshold = threshold,
                       file = file)

  testthat::expect_message(options$getLogConfiguration(),
                           "[D2MCSOptions][getLogConfiguration][INFO] Log configuration:
	- Threshold: INFO
	- Console log status: Disabled
	- File log status: Disabled",
                           fixed = TRUE)

  d2mcs.Options$configureLog(console = console,
                             threshold = threshold,
                             file = file)

  testthat::expect_silent(d2mcs.log(message = "empty", level = "INFO"))
})

testthat::teardown({
  d2mcs.Options$configureLog()
})
