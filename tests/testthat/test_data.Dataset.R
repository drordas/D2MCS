testthat::test_that("Dataset: initialize", {

  file.path <-  file.path("resourceFiles", "data", "hcc-data-complete-balanced.csv")
  testthat::expect_is(Dataset$new(filepath = file.path,
                                  header = TRUE,
                                  sep = ",",
                                  skip = 1,
                                  target.class = 50,
                                  positive.class = 1,
                                  normalize.names = TRUE,
                                  string.as.factor = FALSE,
                                  ignore.columns = NULL),
                      "Dataset")

  testthat::expect_is(Dataset$new(filepath = file.path,
                                  header = TRUE,
                                  sep = ",",
                                  skip = 1,
                                  target.class = 50,
                                  positive.class = 1,
                                  normalize.names = FALSE,
                                  string.as.factor = FALSE,
                                  ignore.columns = NULL),
                      "Dataset")

  testthat::expect_is(Dataset$new(filepath = file.path,
                                  header = FALSE,
                                  sep = ",",
                                  skip = 1,
                                  target.class = 50,
                                  positive.class = 1,
                                  normalize.names = TRUE,
                                  string.as.factor = FALSE,
                                  ignore.columns = NULL),
                      "Dataset")

  testthat::expect_is(Dataset$new(filepath = file.path,
                                  header = FALSE,
                                  sep = ",",
                                  skip = 1,
                                  target.class = 50,
                                  positive.class = 1,
                                  normalize.names = TRUE,
                                  string.as.factor = FALSE,
                                  ignore.columns = c(1, 2)),
                      "Dataset")

  testthat::expect_is(Dataset$new(filepath = file.path,
                                  header = TRUE,
                                  sep = ",",
                                  skip = 1,
                                  target.class = "Gender",
                                  positive.class = 1,
                                  normalize.names = TRUE,
                                  string.as.factor = FALSE,
                                  ignore.columns = NULL),
                      "Dataset")

})

testthat::test_that("Dataset: initialize function checks parameter type", {

  testthat::expect_error(Dataset$new(filepath = NULL,
                                     header = TRUE,
                                     sep = ",",
                                     skip = 1,
                                     target.class = 50,
                                     positive.class = 1,
                                     normalize.names = TRUE,
                                     string.as.factor = FALSE,
                                     ignore.columns = NULL),
                         "[Dataset][FATAL] Corpus cannot be found at defined location. Aborting...",
                         fixed = TRUE)

  testthat::expect_error(Dataset$new(filepath = "wrongFile.csv",
                                     header = TRUE,
                                     sep = ",",
                                     skip = 1,
                                     target.class = 50,
                                     positive.class = 1,
                                     normalize.names = TRUE,
                                     string.as.factor = FALSE,
                                     ignore.columns = NULL),
                         "[Dataset][FATAL] Corpus cannot be found at defined location. Aborting...",
                         fixed = TRUE)

  file.path <-  file.path("resourceFiles", "data", "hcc-data-complete-balanced.csv")

  testthat::expect_error(Dataset$new(filepath = file.path,
                                     header = TRUE,
                                     sep = ",",
                                     skip = 1,
                                     target.class = NULL,
                                     positive.class = 1,
                                     normalize.names = TRUE,
                                     string.as.factor = FALSE,
                                     ignore.columns = NULL),
                         "[Dataset][FATAL] Target class parameter should be defined. Aborting...",
                         fixed = TRUE)

  testthat::expect_error(Dataset$new(filepath = file.path,
                                     header = TRUE,
                                     sep = ",",
                                     skip = 1,
                                     target.class = 50,
                                     positive.class = NULL,
                                     normalize.names = TRUE,
                                     string.as.factor = FALSE,
                                     ignore.columns = NULL),
                         "[Dataset][FATAL] Positive class parameter should be defined. Aborting...",
                         fixed = TRUE)

  testthat::expect_error(Dataset$new(filepath = file.path,
                                     header = TRUE,
                                     sep = ",",
                                     skip = 1,
                                     target.class = list(),
                                     positive.class = 1,
                                     normalize.names = TRUE,
                                     string.as.factor = FALSE,
                                     ignore.columns = NULL),
                         "[Dataset][FATAL] Target class is incorrect. Must contain a numerical or character value. Aborting...",
                         fixed = TRUE)

  testthat::expect_error(Dataset$new(filepath = file.path,
                                     header = FALSE,
                                     sep = ",",
                                     skip = 1,
                                     target.class = "a",
                                     positive.class = 1,
                                     normalize.names = TRUE,
                                     string.as.factor = FALSE,
                                     ignore.columns = NULL),
                         "[Dataset][FATAL] Cannot name target class without columns names. Aborting...",
                         fixed = TRUE)

  testthat::expect_error(Dataset$new(filepath = file.path,
                                     header = TRUE,
                                     sep = ",",
                                     skip = 1,
                                     target.class = -2,
                                     positive.class = 1,
                                     normalize.names = TRUE,
                                     string.as.factor = FALSE,
                                     ignore.columns = NULL),
                         "[Dataset][FATAL] Class index exceeds dataset limits. Must be between 1 and 50. Aborting...",
                         fixed = TRUE)

  testthat::expect_error(Dataset$new(filepath = file.path,
                                     header = TRUE,
                                     sep = ",",
                                     skip = 1,
                                     target.class = "A",
                                     positive.class = 1,
                                     normalize.names = TRUE,
                                     string.as.factor = FALSE,
                                     ignore.columns = NULL),
                         "[Dataset][FATAL] There are no columns named as 'A'. Aborting...",
                         fixed = TRUE)

  testthat::expect_error(Dataset$new(filepath = file.path,
                                     header = TRUE,
                                     sep = ",",
                                     skip = 1,
                                     target.class = 51,
                                     positive.class = 1,
                                     normalize.names = TRUE,
                                     string.as.factor = FALSE,
                                     ignore.columns = NULL),
                         "[Dataset][FATAL] Class index exceeds dataset limits. Must be between 1 and 50. Aborting...",
                         fixed = TRUE)


  testthat::expect_error(Dataset$new(filepath = file.path,
                                     header = TRUE,
                                     sep = ",",
                                     skip = 1,
                                     target.class = 50,
                                     positive.class = 3,
                                     normalize.names = TRUE,
                                     string.as.factor = FALSE,
                                     ignore.columns = NULL),
                         "[Dataset][FATAL] Positive class value not found. Aborting...",
                         fixed = TRUE)

})

testthat::test_that("Dataset: getColumnNames function works", {

  file.path <-  file.path("resourceFiles", "data", "hcc-data-complete-balanced.csv")
  data <- Dataset$new(filepath = file.path,
                                  header = TRUE,
                                  sep = ",",
                                  skip = 1,
                                  target.class = 50,
                                  positive.class = 1,
                                  normalize.names = TRUE,
                                  string.as.factor = FALSE,
                                  ignore.columns = NULL)

  column.names <- unlist(strsplit(scan(file = file.path, nlines = 1,
                                       what = "character", quiet = TRUE),
                                  split = ","))

  testthat::expect_equal(data$getColumnNames(), column.names)

})

testthat::test_that("Dataset: getDataset function works", {

  file.path <-  file.path("resourceFiles", "data", "hcc-data-complete-balanced.csv")
  data <- Dataset$new(filepath = file.path,
                      header = TRUE,
                      sep = ",",
                      skip = 1,
                      target.class = 50,
                      positive.class = 1,
                      normalize.names = TRUE,
                      string.as.factor = FALSE,
                      ignore.columns = NULL)

  corpus <- read.csv(file = file.path, header = TRUE,
                     skip = 2, sep = ",",  stringsAsFactors = FALSE)

  column.names <- unlist(strsplit(scan(file = file.path, nlines = 1,
                                       what = "character", quiet = TRUE),
                                  split = ","))

  names(corpus) <- column.names

  testthat::expect_equal(data$getDataset(), corpus)

})

testthat::test_that("Dataset: getClassName function works", {

  file.path <-  file.path("resourceFiles", "data", "hcc-data-complete-balanced.csv")
  data <- Dataset$new(filepath = file.path,
                      header = TRUE,
                      sep = ",",
                      skip = 1,
                      target.class = 50,
                      positive.class = 1,
                      normalize.names = TRUE,
                      string.as.factor = FALSE,
                      ignore.columns = NULL)

  column.names <- unlist(strsplit(scan(file = file.path, nlines = 1,
                                       what = "character", quiet = TRUE),
                                  split = ","))

  testthat::expect_equal(data$getClassName(), column.names[50])

})

testthat::test_that("Dataset: getClassIndex function works", {

  file.path <-  file.path("resourceFiles", "data", "hcc-data-complete-balanced.csv")
  data <- Dataset$new(filepath = file.path,
                      header = TRUE,
                      sep = ",",
                      skip = 1,
                      target.class = 50,
                      positive.class = 1,
                      normalize.names = TRUE,
                      string.as.factor = FALSE,
                      ignore.columns = NULL)

  testthat::expect_equal(data$getClassIndex(), 50)

  data <- Dataset$new(filepath = file.path,
                      header = TRUE,
                      sep = ",",
                      skip = 1,
                      target.class = 50,
                      positive.class = 1,
                      normalize.names = TRUE,
                      string.as.factor = FALSE,
                      ignore.columns = c(2, 3))

  testthat::expect_equal(data$getClassIndex(), 48)

})

testthat::test_that("Dataset: getClassValues function works", {

  file.path <-  file.path("resourceFiles", "data", "hcc-data-complete-balanced.csv")
  data <- Dataset$new(filepath = file.path,
                      header = TRUE,
                      sep = ",",
                      skip = 1,
                      target.class = 50,
                      positive.class = 1,
                      normalize.names = TRUE,
                      string.as.factor = FALSE,
                      ignore.columns = NULL)

  corpus <- read.csv(file = file.path, header = TRUE,
                     skip = 2, sep = ",")

  testthat::expect_equal(data$getClassValues(), as.character(unique(corpus[, 50])))

})

testthat::test_that("Dataset: getPositiveClass function works", {

  file.path <-  file.path("resourceFiles", "data", "hcc-data-complete-balanced.csv")
  data <- Dataset$new(filepath = file.path,
                      header = TRUE,
                      sep = ",",
                      skip = 1,
                      target.class = 50,
                      positive.class = 1,
                      normalize.names = TRUE,
                      string.as.factor = FALSE,
                      ignore.columns = NULL)

  testthat::expect_equal(data$getPositiveClass(), 1)

})

testthat::test_that("Dataset: getNcol function works", {

  file.path <-  file.path("resourceFiles", "data", "hcc-data-complete-balanced.csv")
  data <- Dataset$new(filepath = file.path,
                      header = TRUE,
                      sep = ",",
                      skip = 1,
                      target.class = 50,
                      positive.class = 1,
                      normalize.names = TRUE,
                      string.as.factor = FALSE,
                      ignore.columns = NULL)

  corpus <- read.csv(file = file.path, header = TRUE,
                     skip = 2, sep = ",")

  testthat::expect_equal(data$getNcol(), ncol(corpus))

})

testthat::test_that("Dataset: getNrow function works", {

  file.path <-  file.path("resourceFiles", "data", "hcc-data-complete-balanced.csv")
  data <- Dataset$new(filepath = file.path,
                      header = TRUE,
                      sep = ",",
                      skip = 1,
                      target.class = 50,
                      positive.class = 1,
                      normalize.names = TRUE,
                      string.as.factor = FALSE,
                      ignore.columns = NULL)

  corpus <- read.csv(file = file.path, header = TRUE,
                     skip = 2, sep = ",")

  testthat::expect_equal(data$getNrow(), nrow(corpus))

})

testthat::test_that("Dataset: getClassSummary function works", {

  file.path <-  file.path("resourceFiles", "data", "hcc-data-complete-balanced.csv")
  data <- Dataset$new(filepath = file.path,
                      header = TRUE,
                      sep = ",",
                      skip = 1,
                      target.class = 50,
                      positive.class = 1,
                      normalize.names = TRUE,
                      string.as.factor = FALSE,
                      ignore.columns = NULL)

  corpus <- read.csv(file = file.path, header = TRUE,
                     skip = 2, sep = ",", stringsAsFactors = FALSE)


  testthat::expect_equal(data$getClassSummary(), data.frame("N. Instances" = as.matrix(table(corpus[, 50]))))

})

testthat::test_that("Dataset: getRemovedColumns function works", {

  file.path <-  file.path("resourceFiles", "data", "hcc-data-complete-balanced.csv")
  data <- Dataset$new(filepath = file.path,
                      header = TRUE,
                      sep = ",",
                      skip = 1,
                      target.class = 50,
                      positive.class = 1,
                      normalize.names = TRUE,
                      string.as.factor = FALSE,
                      ignore.columns = NULL)

  corpus <- read.csv(file = file.path, header = TRUE,
                     skip = 2, sep = ",")

  testthat::expect_equal(data$getRemovedColumns(), list())

})

testthat::test_that("Dataset: setPositiveClass function works", {

  file.path <-  file.path("resourceFiles", "data", "hcc-data-complete-balanced.csv")
  data <- Dataset$new(filepath = file.path,
                      header = TRUE,
                      sep = ",",
                      skip = 1,
                      target.class = 50,
                      positive.class = 1,
                      normalize.names = TRUE,
                      string.as.factor = FALSE,
                      ignore.columns = NULL)

  testthat::expect_invisible(data$setPositiveClass(0))

})

testthat::test_that("Dataset: setPositiveClass function checks parameter type", {

  file.path <-  file.path("resourceFiles", "data", "hcc-data-complete-balanced.csv")
  data <- Dataset$new(filepath = file.path,
                      header = TRUE,
                      sep = ",",
                      skip = 1,
                      target.class = 50,
                      positive.class = 1,
                      normalize.names = TRUE,
                      string.as.factor = FALSE,
                      ignore.columns = NULL)

  testthat::expect_message(data$setPositiveClass(3),
                           "[Dataset][ERROR] Positive class value not found. Task not performed",
                           fixed = TRUE)

})

testthat::test_that("Dataset: setClassIndex function works", {

  file.path <-  file.path("resourceFiles", "data", "hcc-data-complete-balanced.csv")
  data <- Dataset$new(filepath = file.path,
                      header = TRUE,
                      sep = ",",
                      skip = 1,
                      target.class = 50,
                      positive.class = 1,
                      normalize.names = TRUE,
                      string.as.factor = FALSE,
                      ignore.columns = NULL)

  testthat::expect_invisible(data$setClassIndex(44, 3))

})

testthat::test_that("Dataset: setPositiveClass function checks parameter type", {

  file.path <-  file.path("resourceFiles", "data", "hcc-data-complete-balanced.csv")
  data <- Dataset$new(filepath = file.path,
                      header = TRUE,
                      sep = ",",
                      skip = 1,
                      target.class = 50,
                      positive.class = 1,
                      normalize.names = TRUE,
                      string.as.factor = FALSE,
                      ignore.columns = NULL)

  testthat::expect_message(data$setClassIndex(52, 3),
                           "[Dataset][ERROR] Class index exceeds dataset limits. Must be between 1 and 50. Task not performed",
                           fixed = TRUE)

  data <- Dataset$new(filepath = file.path,
                      header = TRUE,
                      sep = ",",
                      skip = 1,
                      target.class = 50,
                      positive.class = 1,
                      normalize.names = TRUE,
                      string.as.factor = FALSE,
                      ignore.columns = NULL)

  testthat::expect_message(data$setClassIndex(50, 3),
                           "[Dataset][ERROR] Positive class value not found. Task not performed",
                           fixed = TRUE)
})

testthat::test_that("Dataset: setClassName function works", {

  file.path <-  file.path("resourceFiles", "data", "hcc-data-complete-balanced.csv")
  data <- Dataset$new(filepath = file.path,
                      header = TRUE,
                      sep = ",",
                      skip = 1,
                      target.class = 50,
                      positive.class = 1,
                      normalize.names = TRUE,
                      string.as.factor = FALSE,
                      ignore.columns = NULL)

  testthat::expect_invisible(data$setClassName("Nodule", 3))
})

testthat::test_that("Dataset: setPositiveClass function checks parameter type", {

  file.path <-  file.path("resourceFiles", "data", "hcc-data-complete-balanced.csv")
  data <- Dataset$new(filepath = file.path,
                      header = TRUE,
                      sep = ",",
                      skip = 1,
                      target.class = 50,
                      positive.class = 1,
                      normalize.names = TRUE,
                      string.as.factor = FALSE,
                      ignore.columns = NULL)

  testthat::expect_message(data$setClassName("WrongCol", 3),
                           "[Dataset][ERROR] Class name not found. Task not performed",
                           fixed = TRUE)

  data <- Dataset$new(filepath = file.path,
                      header = TRUE,
                      sep = ",",
                      skip = 1,
                      target.class = 50,
                      positive.class = 1,
                      normalize.names = TRUE,
                      string.as.factor = FALSE,
                      ignore.columns = NULL)

  testthat::expect_message(data$setClassName("Nodule", 20),
                           "[Dataset][ERROR] Positive class value not found. Task not performed",
                           fixed = TRUE)
})

testthat::test_that("Dataset: cleanData function works", {

  file.path <-  file.path("resourceFiles", "data", "hcc-data-complete-balanced.csv")
  data <- Dataset$new(filepath = file.path,
                      header = TRUE,
                      sep = ",",
                      skip = 1,
                      target.class = 50,
                      positive.class = 1,
                      normalize.names = TRUE,
                      string.as.factor = FALSE,
                      ignore.columns = NULL)

  testthat::expect_invisible(data$cleanData(remove.funcs = NULL,
                                          remove.na = FALSE,
                                          remove.const = FALSE))

  testthat::expect_message(data$cleanData(remove.funcs = NULL,
                                          remove.na = TRUE,
                                          remove.const = TRUE),
                           "[Dataset][INFO] Total 0 NA columns were succesfully removed",
                           fixed = TRUE,
                           all = FALSE)

  testthat::expect_message(data$cleanData(remove.funcs = NULL,
                                          remove.na = TRUE,
                                          remove.const = TRUE),
                           "[Dataset][INFO] Total 0 const columns were succesfully removed",
                           fixed = TRUE,
                           all = FALSE)

  testthat::expect_message(data$cleanData(remove.funcs = NULL,
                                          remove.na = FALSE,
                                          remove.const = TRUE),
                           "[Dataset][INFO] Total 0 const columns were succesfully removed",
                           fixed = TRUE)

  testthat::expect_message(data$cleanData(remove.funcs = NULL,
                                          remove.na = TRUE,
                                          remove.const = FALSE),
                           "[Dataset][INFO] Total 0 NA columns were succesfully removed",
                           fixed = TRUE)


})

testthat::test_that("Dataset: removeColumns function works", {

  file.path <-  file.path("resourceFiles", "data", "hcc-data-complete-balanced.csv")
  data <- Dataset$new(filepath = file.path,
                      header = TRUE,
                      sep = ",",
                      skip = 1,
                      target.class = 50,
                      positive.class = 1,
                      normalize.names = TRUE,
                      string.as.factor = FALSE,
                      ignore.columns = NULL)

  testthat::expect_message(data$removeColumns(columns = c("Symptoms")),
                           "[Dataset][INFO] Total 1 columns were succesfully removed",
                           fixed = TRUE,
                           all = FALSE)

  testthat::expect_message(data$removeColumns(columns = c("a")),
                           "[Dataset][ERROR] Defined column(s) are not valid. Task not performed",
                           fixed = TRUE)

  testthat::expect_message(data$removeColumns(columns = 50),
                           "[Dataset][ERROR] Selected columns are not valid. Must be between [1-49]. Task not performed",
                           fixed = TRUE,
                           all = FALSE)

  testthat::expect_message(data$removeColumns(columns = 1),
                           "[Dataset][INFO] Total 1 columns were succesfully removed",
                           fixed = TRUE,
                           all = FALSE)

})

testthat::test_that("Dataset: createPartitions function works", {

  file.path <-  file.path("resourceFiles", "data", "hcc-data-complete-balanced.csv")
  data <- Dataset$new(filepath = file.path,
                      header = TRUE,
                      sep = ",",
                      skip = 1,
                      target.class = 50,
                      positive.class = 1,
                      normalize.names = TRUE,
                      string.as.factor = FALSE,
                      ignore.columns = NULL)

  testthat::expect_message(data$createPartitions(num.folds = 4,
                                                 percent.folds = "wrong",
                                                 class.balance = TRUE),
                           "[Dataset][INFO] Perfoming dataset partitioning into 4 groups",
                           fixed = TRUE)

  testthat::expect_message(data$createPartitions(num.folds = 4,
                                                 percent.folds = c(.25, .25, .25, .25),
                                                 class.balance = TRUE),
                           "[Dataset][INFO] Perfoming dataset partitioning into 4 groups",
                           fixed = TRUE)

  testthat::expect_message(data$createPartitions(num.folds = 4,
                                                 percent.folds = c(25, 25, 25, 25),
                                                 class.balance = TRUE),
                           "[Dataset][INFO] Perfoming dataset partitioning into 4 groups",
                           fixed = TRUE)

  testthat::expect_message(data$createPartitions(num.folds = 4,
                                                 percent.folds = c(25, 25, 25, 25),
                                                 class.balance = TRUE),
                           "[Dataset][INFO] Perfoming dataset partitioning into 4 groups",
                           fixed = TRUE)

  testthat::expect_message(data$createPartitions(num.folds = 10,
                                                 percent.folds = c(10, 10, 10, 10, 10,
                                                                   10, 10, 10, 10, 10),
                                                 class.balance = TRUE),
                           "[Dataset][INFO] Perfoming dataset partitioning into 10 groups",
                           fixed = TRUE)

  testthat::expect_message(data$createPartitions(num.folds = 10,
                                                 percent.folds = c(10, 10, 10, 10, 10,
                                                                   10, 10, 10, 10, 10),
                                                 class.balance = FALSE),
                           "[Dataset][INFO] Perfoming dataset partitioning into 10 groups",
                           fixed = TRUE)

  testthat::expect_message(data$createPartitions(num.folds = NULL,
                                                 percent.folds = c(25, 25, 25, 25),
                                                 class.balance = TRUE),
                           "[Dataset][INFO] Perfoming dataset partitioning into 4 groups",
                           fixed = TRUE)

  testthat::expect_message(data$createPartitions(num.folds = NULL,
                                                 percent.folds = c(.25, .25, .25, .25),
                                                 class.balance = TRUE),
                           "[Dataset][INFO] Perfoming dataset partitioning into 4 groups",
                           fixed = TRUE)

  testthat::expect_message(data$createPartitions(num.folds = NULL,
                                                 percent.folds = c(25, 25, 25, 25),
                                                 class.balance = FALSE),
                           "[Dataset][INFO] Perfoming dataset partitioning into 4 groups",
                           fixed = TRUE)

  testthat::expect_message(data$createPartitions(num.folds = NULL,
                                                 percent.folds = c(10, 10, 10, 10, 10,
                                                                   10, 10, 10, 10, 10),
                                                 class.balance = FALSE),
                           "[Dataset][INFO] Perfoming dataset partitioning into 10 groups",
                           fixed = TRUE)

})

testthat::test_that("Dataset: createPartitions function checks parameter type", {

  file.path <-  file.path("resourceFiles", "data", "hcc-data-complete-balanced.csv")
  data <- Dataset$new(filepath = file.path,
                      header = TRUE,
                      sep = ",",
                      skip = 1,
                      target.class = 50,
                      positive.class = 1,
                      normalize.names = TRUE,
                      string.as.factor = FALSE,
                      ignore.columns = NULL)

  testthat::expect_message(data$createPartitions(num.folds = NULL,
                                                 percent.folds = NULL,
                                                 class.balance = TRUE),
                           "[Dataset][WARNING] Parameters are invalid. Assuming division with default k=10 folds",
                           fixed = TRUE)

  testthat::expect_message(data$createPartitions(num.folds = "wrong",
                                                 percent.folds = NULL,
                                                 class.balance = TRUE),
                           "[Dataset][WARNING] Parameters are invalid. Assuming division with default k=10 folds",
                           fixed = TRUE)

  testthat::expect_message(data$createPartitions(num.folds = 1:2,
                                                 percent.folds = NULL,
                                                 class.balance = TRUE),
                           "[Dataset][WARNING] Parameters are invalid. Assuming division with default k=10 folds",
                           fixed = TRUE)

  testthat::expect_message(data$createPartitions(num.folds = NULL,
                                                 percent.folds = "wrong",
                                                 class.balance = TRUE),
                           "[Dataset][WARNING] Parameters are invalid. Assuming division with default k=10 folds",
                           fixed = TRUE)

  testthat::expect_error(data$createPartitions(num.folds = 4,
                                               percent.folds = c(.25, .25, .25, .25),
                                               class.balance = "A"),
                         "[Dataset][FATAL] class.balance not defined. Aborting...",
                         fixed = TRUE)

  testthat::expect_message(data$createPartitions(num.folds = 4,
                                                 percent.folds = c(30, 25, 25, 25),
                                                 class.balance = TRUE),
                           "[Dataset][ERROR] Fold partition and/or probability mismatch. Task not performed",
                           fixed = TRUE)

  testthat::expect_message(data$createPartitions(num.folds = 4,
                                                 percent.folds = c(.30, .25, .25, .25),
                                                 class.balance = TRUE),
                           "[Dataset][ERROR] Fold partition and/or probability mismatch. Task not performed",
                           fixed = TRUE)

  testthat::expect_message(data$createPartitions(num.folds = 20,
                                                 percent.folds = c(25, 25, 25, 25),
                                                 class.balance = TRUE),
                           "[Dataset][ERROR] Fold partition and/or probability mismatch. Task not performed",
                           fixed = TRUE)

  testthat::expect_message(data$createPartitions(num.folds = 20,
                                                 percent.folds = 3,
                                                 class.balance = TRUE),
                           "[Dataset][ERROR] Fold partition and/or probability mismatch. Task not performed",
                           fixed = TRUE)

  testthat::expect_message(data$createPartitions(num.folds = NULL,
                                                 percent.folds = c(30, 25, 25, 25),
                                                 class.balance = TRUE),
                           "[Dataset][ERROR] Cannot perform partition process. Task not performed",
                           fixed = TRUE)

  testthat::expect_message(data$createPartitions(num.folds = NULL,
                                                 percent.folds = c(.30, .25, .25, .25),
                                                 class.balance = TRUE),
                           "[Dataset][ERROR] Cannot perform partition process. Task not performed",
                           fixed = TRUE)
})

testthat::test_that("Dataset: createSubset function works", {

  file.path <-  file.path("resourceFiles", "data", "hcc-data-complete-balanced.csv")
  data <- Dataset$new(filepath = file.path,
                      header = TRUE,
                      sep = ",",
                      skip = 1,
                      target.class = 50,
                      positive.class = 1,
                      normalize.names = TRUE,
                      string.as.factor = FALSE,
                      ignore.columns = NULL)
  set.seed(2)
  data$createPartitions(num.folds = 4)

  testthat::expect_is(data$createSubset(num.folds = 4,
                                        column.id = NULL,
                                        opts = list(remove.na = TRUE, remove.const = TRUE)),
                     "Subset")
  testthat::expect_message(data$createSubset(num.folds = 4,
                                             column.id = NULL,
                                             opts = list(remove.na = TRUE, remove.const = FALSE)),
                           "[Dataset][INFO] Removed columns containing NA values (total of 0)",
                           fixed = TRUE)

  testthat::expect_is(data$createSubset(num.folds = 4,
                                        column.id = NULL,
                                        opts = list(remove.na = TRUE, remove.const = FALSE)),
                     "Subset")


  testthat::expect_message(data$createSubset(num.folds = 4,
                                             column.id = NULL,
                                             opts = list(remove.na = TRUE, remove.const = TRUE)),
                           "[Dataset][INFO] Removed columns containing NA values (total of 0)",
                           fixed = TRUE)

  testthat::expect_is(data$createSubset(num.folds = 4,
                                        column.id = NULL,
                                        opts = list(remove.na = TRUE, remove.const = TRUE)),
                      "Subset")

  testthat::expect_message(data$createSubset(num.folds = 4,
                                             column.id = NULL,
                                             opts = list(remove.na = TRUE, remove.const = TRUE)),
                           "[Dataset][INFO] Removed columns containing constant values (total of 1)",
                           fixed = TRUE,
                           all = FALSE)

  testthat::expect_is(data$createSubset(num.folds = 4,
                                        column.id = NULL,
                                        opts = list(remove.na = TRUE, remove.const = TRUE)),
                      "Subset")

  testthat::expect_message(data$createSubset(num.folds = 4,
                                             column.id = NULL,
                                             opts = list(remove.na = FALSE, remove.const = TRUE)),
                           "[Dataset][INFO] Removed columns containing constant values (total of 1)",
                           fixed = TRUE)

  testthat::expect_is(data$createSubset(num.folds = 4,
                                        column.id = NULL,
                                        opts = list(remove.na = FALSE, remove.const = TRUE)),
                      "Subset")

  testthat::expect_message(data$createSubset(num.folds = 4,
                                             column.id = "a",
                                             opts = list(remove.na = FALSE, remove.const = FALSE)),
                           "[Dataset][WARNING] Feature identifier is not correct. Ignoring value",
                           fixed = TRUE)
})

testthat::test_that("Dataset: createSubset function checks parameter type", {

  file.path <-  file.path("resourceFiles", "data", "hcc-data-complete-balanced.csv")
  data <- Dataset$new(filepath = file.path,
                      header = TRUE,
                      sep = ",",
                      skip = 1,
                      target.class = 50,
                      positive.class = 1,
                      normalize.names = TRUE,
                      string.as.factor = FALSE,
                      ignore.columns = NULL)

  testthat::expect_message(data$createSubset(num.folds = NULL,
                                             opts = list(remove.na = TRUE, remove.const = FALSE)),
                           "[Dataset][ERROR] Dataset distribution is null. Task not performed",
                           fixed = TRUE)

  testthat::expect_null(data$createSubset(num.folds = NULL,
                                          opts = list(remove.na = TRUE, remove.const = FALSE)))

  data$createPartitions(num.folds = 4)

  testthat::expect_message(data$createSubset(num.folds = NULL,
                                             opts = list(remove.na = TRUE, remove.const = FALSE)),
                           "[Dataset][WARNING] Incorrect number of folds. Must be between 1 and 4. Assuming whole dataset",
                           fixed = TRUE)

  testthat::expect_message(data$createSubset(num.folds = "a",
                                             opts = list(remove.na = TRUE, remove.const = FALSE)),
                           "[Dataset][WARNING] Incorrect number of folds. Must be between 1 and 4. Assuming whole dataset",
                           fixed = TRUE)

  testthat::expect_message(data$createSubset(num.folds = 8,
                                             opts = list(remove.na = TRUE, remove.const = FALSE)),
                           "[Dataset][WARNING] Incorrect number of folds. Must be between 1 and 4. Assuming whole dataset",
                           fixed = TRUE)
})

testthat::test_that("Dataset: createTrain function works", {

  file.path <-  file.path("resourceFiles", "data", "hcc-data-complete-balanced.csv")
  data <- Dataset$new(filepath = file.path,
                      header = TRUE,
                      sep = ",",
                      skip = 1,
                      target.class = 50,
                      positive.class = 1,
                      normalize.names = TRUE,
                      string.as.factor = FALSE,
                      ignore.columns = NULL)

  data$createPartitions(num.folds = 4)

  testthat::expect_is(data$createTrain(num.folds = 4,
                                       opts = list(remove.na = TRUE, remove.const = TRUE)),
                      "Trainset")

  testthat::expect_message(data$createTrain(num.folds = 4,
                                            opts = list(remove.na = TRUE, remove.const = FALSE)),
                           "[Dataset][INFO] Removed columns containing NA values (total of 0)",
                           fixed = TRUE)

  testthat::expect_is(data$createTrain(num.folds = 4,
                                       opts = list(remove.na = TRUE, remove.const = FALSE)),
                      "Trainset")


  testthat::expect_message(data$createTrain(num.folds = 4,
                                            opts = list(remove.na = TRUE, remove.const = TRUE)),
                           "[Dataset][INFO] Removed columns containing NA values (total of 0)",
                           fixed = TRUE)

  testthat::expect_is(data$createTrain(num.folds = 4,
                                          opts = list(remove.na = TRUE, remove.const = TRUE)),
                         "Trainset")

  testthat::expect_message(data$createTrain(num.folds = 4,
                                            opts = list(remove.na = TRUE, remove.const = TRUE)),
                           "[Dataset][INFO] Removed columns containing constant values (total of 0)",
                           fixed = TRUE)

  testthat::expect_is(data$createTrain(num.folds = 4,
                                       opts = list(remove.na = TRUE, remove.const = TRUE)),
                      "Trainset")

  testthat::expect_message(data$createTrain(num.folds = 4,
                                            opts = list(remove.na = FALSE, remove.const = TRUE)),
                           "[Dataset][INFO] Removed columns containing constant values (total of 0)",
                           fixed = TRUE)

  testthat::expect_is(data$createTrain(num.folds = 4,
                                       opts = list(remove.na = FALSE, remove.const = TRUE)),
                      "Trainset")
})

testthat::test_that("Dataset: createTrain function checks parameter type", {

  file.path <-  file.path("resourceFiles", "data", "hcc-data-complete-balanced.csv")
  data <- Dataset$new(filepath = file.path,
                      header = TRUE,
                      sep = ",",
                      skip = 1,
                      target.class = 50,
                      positive.class = 1,
                      normalize.names = TRUE,
                      string.as.factor = FALSE,
                      ignore.columns = NULL)

  testthat::expect_message(data$createTrain(num.folds = NULL,
                                            opts = list(remove.na = TRUE, remove.const = FALSE)),
                           "[Dataset][ERROR] Dataset distribution is null. Task not performed",
                           fixed = TRUE)

  testthat::expect_null(data$createTrain(num.folds = NULL,
                                         opts = list(remove.na = TRUE, remove.const = FALSE)))

  data$createPartitions(num.folds = 4)

  testthat::expect_message(data$createTrain(num.folds = NULL,
                                            opts = list(remove.na = TRUE, remove.const = FALSE)),
                           "[Dataset][WARNING] Incorrect number of folds. Must be between 1 and 4. Assuming whole dataset",
                           fixed = TRUE)

  testthat::expect_message(data$createTrain(num.folds = "a",
                                            opts = list(remove.na = TRUE, remove.const = FALSE)),
                           "[Dataset][WARNING] Incorrect number of folds. Must be between 1 and 4. Assuming whole dataset",
                           fixed = TRUE)

  testthat::expect_message(data$createTrain(num.folds = 8,
                                            opts = list(remove.na = TRUE, remove.const = FALSE)),
                           "[Dataset][WARNING] Incorrect number of folds. Must be between 1 and 4. Assuming whole dataset",
                           fixed = TRUE)
})
