testthat::setup({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})

testthat::test_that("GenericModelFit: initialize function works", {

  instances <- data.frame(c(1, 2), c(2, 2))

  colnames(instances) <- c("C1", "Class")

  testthat::expect_is(GenericModelFit$new(), "GenericModelFit")
})

testthat::teardown({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})

testthat::setup({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})

testthat::test_that("GenericModelFit: createFormula function works", {

  instances <- data.frame(c(1, 2), c(2, 2))

  colnames(instances) <- c("C1", "Class")

  testthat::expect_error(GenericModelFit$new()$createFormula(instances = instances,
                                                             class.name = "Class"),
                         "[GenericModelFit][createFormula][FATAL] Class is abstract. Method should be defined in inherited class. Aborting...",
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

testthat::test_that("GenericModelFit: createRecipe function works", {

  instances <- data.frame(c(1, 2), c(2, 2))

  colnames(instances) <- c("C1", "Class")

  testthat::expect_error(GenericModelFit$new()$createRecipe(instances = instances,
                                                            class.name = "Class"),
                         "[GenericModelFit][createRecipe][FATAL] Class is abstract. Method should be defined in inherited class. Aborting...",
                         fixed = TRUE)
})

testthat::teardown({
  d2mcs.Options$reset()
  d2mcs.Options$configureLog()
})
