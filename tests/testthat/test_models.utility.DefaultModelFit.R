testthat::test_that("DefaultModelFit: initialize", {

  instances <- data.frame(c(1, 2), c(2, 2))

  colnames(instances) <- c("C1", "Class")


  testthat::expect_is(DefaultModelFit$new(instances = instances,
                                          class.name = "Class"),
                      "DefaultModelFit")
})

testthat::test_that("DefaultModelFit: createFormula function works", {

  instances <- data.frame(c(1, 2), c(2, 2))

  colnames(instances) <- c("C1", "Class")

  testthat::expect_is(DefaultModelFit$new(instances = instances,
                                             class.name = "Class")$createFormula(simplify = FALSE),
                        "formula")

  testthat::expect_is(DefaultModelFit$new(instances = instances,
                                            class.name = "Class")$createFormula(simplify = TRUE),
                        "formula")
})

testthat::test_that("DefaultModelFit: createRecipe function works", {

  instances <- data.frame(c(1, 2), c(2, 2))

  colnames(instances) <- c("C1", "Class")

  testthat::expect_is(DefaultModelFit$new(instances = instances,
                                             class.name = "Class")$createRecipe(),
                        "recipe")
})
