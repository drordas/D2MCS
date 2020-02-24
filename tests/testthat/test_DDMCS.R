testthat::test_that("DDMCS: initialize", {

  dir.path <- file.path("resourceFiles", "DDMCS")
  num.core <- 1
  socket.type <- "PSOCK"
  outfile <- NULL
  serialize <- NULL

  testthat::expect_is(DDMCS$new(dir.path = dir.path,
                                num.core = num.core,
                                socket.type = socket.type,
                                outfile = outfile,
                                serialize = serialize),
                      "DDMCS")

  testthat::expect_is(DDMCS$new(dir.path = dir.path,
                                num.core = 2,
                                socket.type = socket.type,
                                outfile = outfile,
                                serialize = TRUE),
                      "DDMCS")

  unlink(file.path("resourceFiles", "DDMCS"), recursive = TRUE, force = TRUE)

})

testthat::test_that("DDMCS: initialize checks parameter type", {

  dir.path <- NULL
  num.core <- NULL
  socket.type <- "PSOCK"
  outfile <- NULL
  serialize <- NULL

  testthat::expect_error(DDMCS$new(dir.path = dir.path,
                                   num.core = num.core,
                                   socket.type = socket.type,
                                   outfile = outfile,
                                   serialize = serialize),
                         "[DDMCS][FATAL] Path to store ML models should be defined",
                         fixed = TRUE)

  dir.path <- file.path("resourceFiles", "DDMCS")
  num.core <- NULL
  socket.type <- "PSOCK"
  outfile <- file.path("resourceFiles", "DDMCS", "outfile", "null")
  serialize <- NULL

  testthat::expect_message(DDMCS$new(dir.path = dir.path,
                                     num.core = num.core,
                                     socket.type = socket.type,
                                     outfile = outfile,
                                     serialize = serialize),
                           paste0("[DDMCS][INFO] Logs path not defined '", outfile, "' does not exist. Creating..."),
                           fixed = TRUE,
                           all = FALSE)
  file.remove(outfile)

  dir.path <- file.path("resourceFiles", "DDMCS")
  num.core <- NULL
  socket.type <- "wrong"
  outfile <- NULL
  serialize <- NULL

  testthat::expect_message(DDMCS$new(dir.path = dir.path,
                                     num.core = num.core,
                                     socket.type = socket.type,
                                     outfile = outfile,
                                     serialize = serialize),
                           "[DDMCS][WARNING] Invalid socket type. Assuming 'PSOCK' cluster",
                           fixed = TRUE)

  dir.path <- file.path("resourceFiles", "DDMCS")
  num.core <- NULL
  socket.type <- "PSOCK"
  outfile <- NULL
  serialize <- NULL

  testthat::expect_message(DDMCS$new(dir.path = dir.path,
                                     num.core = num.core,
                                     socket.type = socket.type,
                                     outfile = outfile,
                                     serialize = serialize),
                           "[DDMCS][WARNING] Invalid serialization option. Assuming not serialization",
                           fixed = TRUE,
                           all = FALSE)

  unlink(file.path("resourceFiles", "DDMCS"), recursive = TRUE, force = TRUE)
})

testthat::test_that("DDMCS: train function works", {
  dir.path <- file.path("resourceFiles", "DDMCS")
  num.core <- 1
  socket.type <- "PSOCK"
  outfile <- NULL
  serialize <- NULL

  ddmcs <- DDMCS$new(dir.path = dir.path,
                     num.core = num.core,
                     socket.type = socket.type,
                     outfile = outfile,
                     serialize = serialize)

  train.set <- readRDS(file.path("resourceFiles", "data", "trainset.rds"))
  train.function <- TwoClass$new(method = "cv", number = 10, savePredictions = "final",
                                  classProbs = TRUE, allowParallel = TRUE, verboseIter = FALSE)
  num.clusters <- NULL
  ex.classifiers <- c("nb", "ranger", "lda", "lda2")
  ig.classifiers <- c()
  metrics <- c("MCC", "PPV")
  saveAllModels <- FALSE

  testthat::expect_is(suppressWarnings(ddmcs$train(train.set = train.set,
                                                   train.function = train.function,
                                                   num.clusters = num.clusters,
                                                   ex.classifiers = ex.classifiers,
                                                   ig.classifiers = ig.classifiers,
                                                   metrics = metrics,
                                                   saveAllModels = saveAllModels)),
                      "TrainOutput")

  unlink(file.path("resourceFiles", "DDMCS"), recursive = TRUE, force = TRUE)
})

testthat::test_that("DDMCS: train function checks parameter types", {

  dir.path <- file.path("resourceFiles", "DDMCS")
  num.core <- 1
  socket.type <- "PSOCK"
  outfile <- NULL
  serialize <- NULL

  ddmcs <- DDMCS$new(dir.path = dir.path,
                     num.core = num.core,
                     socket.type = socket.type,
                     outfile = outfile,
                     serialize = serialize)

  train.set <- readRDS(file.path("resourceFiles", "data", "trainset.rds"))
  train.function <- TwoClass$new(method = "cv", number = 10, savePredictions = "final",
                                  classProbs = TRUE, allowParallel = TRUE, verboseIter = FALSE)
  num.clusters <- NULL
  ex.classifiers <- c("nb", "ranger", "lda", "lda2")
  ig.classifiers <- c()
  metrics <- c("MCC", "PPV")
  saveAllModels <- FALSE

  testthat::expect_error(ddmcs$train(train.set = NULL,
                                     train.function = train.function,
                                     num.clusters = num.clusters,
                                     ex.classifiers = ex.classifiers,
                                     ig.classifiers = ig.classifiers,
                                     metrics = metrics,
                                     saveAllModels = saveAllModels),
                         "[DDMCS][FATAL] Train set parameter must be defined as 'Trainset' type. Aborting...",
                         fixed = TRUE)

  testthat::expect_error(ddmcs$train(train.set = train.set,
                                     train.function = NULL,
                                     num.clusters = num.clusters,
                                     ex.classifiers = ex.classifiers,
                                     ig.classifiers = ig.classifiers,
                                     metrics = metrics,
                                     saveAllModels = saveAllModels),
                         "[DDMCS][FATAL] Train function parameter must be defined as 'TrainFunction' type. Aborting...",
                         fixed = TRUE)

  testthat::expect_message(suppressWarnings(ddmcs$train(train.set = train.set,
                                                        train.function = train.function,
                                                        num.clusters = NULL,
                                                        ex.classifiers = ex.classifiers,
                                                        ig.classifiers = ig.classifiers,
                                                        metrics = metrics,
                                                        saveAllModels = saveAllModels)),
                         "[DDMCS][WARNING] Number of clusters not set (must be numeric or vector). Using all clusters",
                         fixed = TRUE,
                         all = FALSE)

  testthat::expect_message(suppressWarnings(ddmcs$train(train.set = train.set,
                                                        train.function = train.function,
                                                        num.clusters = 10000,
                                                        ex.classifiers = ex.classifiers,
                                                        ig.classifiers = ig.classifiers,
                                                        metrics = metrics,
                                                        saveAllModels = saveAllModels)),
                           "[DDMCS][WARNING] Number of clusters is higher than number of existing clusters. Using all clusters",
                           fixed = TRUE,
                           all = FALSE)

  testthat::expect_message(suppressWarnings(ddmcs$train(train.set = train.set,
                                                        train.function = train.function,
                                                        num.clusters = num.clusters,
                                                        ex.classifiers = ex.classifiers,
                                                        ig.classifiers = c("ranger"),
                                                        metrics = metrics,
                                                        saveAllModels = saveAllModels)),
                           "[DDMCS][INFO] Ignoring '1' M.L models",
                           fixed = TRUE,
                           all = FALSE)

  testthat::expect_error(ddmcs$train(train.set = train.set,
                                     train.function = train.function,
                                     num.clusters = num.clusters,
                                     ex.classifiers = ex.classifiers,
                                     ig.classifiers = ig.classifiers,
                                     metrics = NULL,
                                     saveAllModels = saveAllModels),
                           "[DDMCS][FATAL] Invalid values of metrics",
                           fixed = TRUE)

  unlink(file.path("resourceFiles", "DDMCS"), recursive = TRUE, force = TRUE)
})


testthat::test_that("DDMCS: getAvailableModels function works", {
  dir.path <- file.path("resourceFiles", "DDMCS")
  num.core <- 1
  socket.type <- "PSOCK"
  outfile <- NULL
  serialize <- NULL

  ddmcs <- DDMCS$new(dir.path = dir.path,
                     num.core = num.core,
                     socket.type = socket.type,
                     outfile = outfile,
                     serialize = serialize)

  testthat::expect_is(ddmcs$getAvailableModels(),
                      "data.frame")
})

testthat::test_that("DDMCS: classify function works", {

  dir.path <- file.path("resourceFiles", "DDMCS-classify")
  num.core <- 1
  socket.type <- "PSOCK"
  outfile <- NULL
  serialize <- NULL

  ddmcs <- DDMCS$new(dir.path = dir.path,
                     num.core = num.core,
                     socket.type = socket.type,
                     outfile = outfile,
                     serialize = serialize)

  train.output <- readRDS(file.path("resourceFiles", "data", "trainoutput.rds"))
  subset <- readRDS(file.path("resourceFiles", "data", "subset-DDMCS-classify.rds"))
  voting.types <- c(SingleVoting$new(voting.schemes = c(ClassWeightedVoting$new(cutoff = 0.7),
                                                        ProbAverageWeightedVoting$new(cutoff = 0.7),
                                                        ProbAverageVoting$new(cutoff = 0.7),
                                                        ClassMajorityVoting$new(cutoff = 0.7)),
                    metrics = c("MCC")),
                    CombinedVoting$new(voting.schemes = ClassWeightedVoting$new(),
                                       combined.metrics = MinimizeFP$new(),
                                       methodology = ProbBasedMethodology$new(),
                                       metrics = c("MCC", "PPV")))
  positive.class <- "class0"

  testthat::expect_is(suppressWarnings(ddmcs$classify(train.output = train.output,
                                                      subset = subset,
                                                      voting.types = voting.types,
                                                      positive.class = positive.class)),
                      "ClassificationOutput")


  unlink(file.path("resourceFiles", "DDMCS-classify"), recursive = TRUE, force = TRUE)
})

testthat::test_that("DDMCS: classify function checks type parameter", {

  dir.path <- file.path("resourceFiles", "DDMCS-classify")
  num.core <- 1
  socket.type <- "PSOCK"
  outfile <- NULL
  serialize <- NULL

  ddmcs <- DDMCS$new(dir.path = dir.path,
                     num.core = num.core,
                     socket.type = socket.type,
                     outfile = outfile,
                     serialize = serialize)

  train.output <- readRDS(file.path("resourceFiles", "data", "trainoutput.rds"))
  subset <- readRDS(file.path("resourceFiles", "data", "subset-DDMCS-classify.rds"))
  voting.types <- c(SingleVoting$new(voting.schemes = c(ClassWeightedVoting$new(cutoff = 0.7),
                                                        ProbAverageWeightedVoting$new(cutoff = 0.7),
                                                        ProbAverageVoting$new(cutoff = 0.7),
                                                        ClassMajorityVoting$new(cutoff = 0.7)),
                                     metrics = c("MCC")),
                    CombinedVoting$new(voting.schemes = ClassWeightedVoting$new(),
                                       combined.metrics = MinimizeFP$new(),
                                       methodology = ProbBasedMethodology$new(),
                                       metrics = c("MCC", "PPV")))
  positive.class <- "class0"

  testthat::expect_error(ddmcs$classify(train.output = NULL,
                                        subset = subset,
                                        voting.types = voting.types,
                                        positive.class = positive.class),
                         "[DDMCS][FATAL] Train output parameter must be defined as 'TrainOutput' type. Aborting...",
                         fixed = TRUE)

  testthat::expect_error(ddmcs$classify(train.output = train.output,
                                        subset = NULL,
                                        voting.types = voting.types,
                                        positive.class = positive.class),
                         "[DDMCS][FATAL] Subset parameter must be defined as 'Subset' or 'HDSubset' type. Aborting...",
                         fixed = TRUE)

  testthat::expect_error(ddmcs$classify(train.output = train.output,
                                        subset = subset,
                                        voting.types = NULL,
                                        positive.class = positive.class),
                         "[DDMCS][FATAL] Voting Schemes parameter must be defined as 'SingleVoting' or 'CombinedVoting' types. Aborting...",
                         fixed = TRUE)

  unlink(file.path("resourceFiles", "DDMCS-classify"), recursive = TRUE, force = TRUE)
})
