testthat::test_that("GenericModelFit: initialize", {

  instances <- data.frame(c(1, 2), c(2, 2))

  colnames(instances) <- c("C1", "Class")


  testthat::expect_is(GenericModelFit$new(instances = instances,
                                          class.name = "Class"),
                      "GenericModelFit")
})

testthat::test_that("GenericModelFit: initialize checks parameter type", {

  instances <- NULL

  testthat::expect_error(GenericModelFit$new(instances = instances,
                                             class.name = "Class"),
                         "[GenericModelFit][FATAL] Instances must be a non-empty data.frame. Aborting...",
                         fixed = TRUE)

  instances <- data.frame(c(1, 2), c(2, 2))

  colnames(instances) <- c("C1", "C2")

  testthat::expect_error(GenericModelFit$new(instances = instances,
                                             class.name = "Class"),
                         "[GenericModelFit][FATAL] Class name not included in instances data.frame. Aborting...",
                         fixed = TRUE)
})

testthat::test_that("GenericModelFit: createFormula function works", {

  instances <- data.frame(c(1, 2), c(2, 2))

  colnames(instances) <- c("C1", "Class")

  testthat::expect_error(GenericModelFit$new(instances = instances,
                                             class.name = "Class")$createFormula(),
                         "[GenericModelFit][FATAL] Class is abstract. Method should be defined in inherited class. Aborting...",
                         fixed = TRUE)
})

testthat::test_that("GenericModelFit: createRecipe function works", {

  instances <- data.frame(c(1, 2), c(2, 2))

  colnames(instances) <- c("C1", "Class")

  testthat::expect_error(GenericModelFit$new(instances = instances,
                                             class.name = "Class")$createRecipe(),
                         "[GenericModelFit][FATAL] Class is abstract. Method should be defined in inherited class. Aborting...",
                         fixed = TRUE)
})
