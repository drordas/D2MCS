#' @title <<tittle>>
#'
#' @description DDMCS
#'
#' @docType class
#'
#' @format NULL
#'
#' @details <<details>
#'
#' @seealso \code{\link{Dataset}}
#'
#' @keywords NULL
#'
#' @import R6
#' @importFrom devtools loaded_packages
#'
#' @export DDMCS

DDMCS <- R6::R6Class(
  classname = "DDMCS",
  portable = TRUE,
  public = list(
    #'
    #' @description <<description>>
    #'
    #' @param dir.path <<description>>
    #' @param num.cores <<description>>
    #' @param socket.type <<description>>
    #' @param outfile <<description>>
    #' @param serialize <<description>>
    #'
    #' @import parallel
    #'
    initialize = function(dir.path, num.cores = NULL, socket.type = "PSOCK", outfile = NULL,
                          serialize = FALSE) {

      if (is.null(dir.path) || !is.character(dir.path)) {
        stop("[", class(self)[1], "][FATAL] Path to store ML models should be defined")
      }
      dir.path <- gsub("\\/$", "", dir.path)

      if (is.null(outfile)) {
        message("[", class(self)[1], "][INFO] Path for Log file not defined")
        outfile <- "/dev/null"
      }
      else {
        if (!file.exists(outfile)) {
          dir.create(outfile, recursive = TRUE)
          message("[", class(self)[1], "][INFO] Logs path not defined '",
                  outfile, "' does not exist. Creating...")
        }
      }

      if (!dir.exists(dir.path)) {
        dir.create(dir.path, recursive = TRUE)
        if (dir.exists(dir.path)) {
          message("[", class(self)[1], "][INFO] Directory '", dir.path,
                  "' has been succesfully created")
        } else { stop("[", class(self)[1], "][FALTAL] Cannot create directory '",
                     dir.path, "'") }
      }
      else { message("[", class(self)[1], "][INFO] Directory already exists") }

      if (any(is.null(num.cores), (num.cores >= parallel::detectCores()), num.cores > 10)) {
        if (parallel::detectCores() > 10) {
          message("[", class(self)[1], "][WARNING] Invalid number of cores ",
                  "(1>= num.cores <= 10)")
        } else {
          message("[", class(self)[1], "][WARNING] Invalid number of cores ",
                  "(1>= num.cores < ", parallel::detectCores(), ")")
        }
        cores <- min(max(1, parallel::detectCores() - 2), 10)
        message("[", class(self)[1], "][INFO] Using default number of cores (",
                cores, "/", parallel::detectCores(), ")")
      }
      else {cores <- num.cores}

      if (!socket.type %in% c("PSOCK", "FORK")) {
        message("[", class(self)[1], "][WARNING] Invalid socket type. ",
                "Assuming 'PSOCK' cluster")
        socket <- "PSOCK"
      }
      else { socket <- socket.type }

      if (!is.logical(serialize)) {
        message("[", class(self)[1], "][WARNING] Invalid serialization ",
                "option. Assuming not serialization")
        xdr <- FALSE
      }
      else xdr <- serialize

      private$availableModels <- private$loadAvailableModels()
      private$path <- dir.path

      private$cluster.conf <- list(cores = cores, socket = socket,
                                    outfile = outfile, xdr = xdr)
      private$cluster.obj <- NULL
    },
    #'
    #' @description <<description>>
    #'
    #' @param train.set <<description>>
    #' @param train.function <<description>>
    #' @param num.clusters <<description>>
    #' @param ex.classifiers <<description>>
    #' @param ig.classifiers <<description>>
    #' @param metrics <<description>>
    #' @param saveAllModels <<description>>
    #'
    #' @return <<description>>
    #'
    #' @import parallel
    #'
    train = function(train.set, train.function, num.clusters = NULL,
                     ex.classifiers = c(), ig.classifiers = c(),
                     metrics = NULL, saveAllModels = FALSE) {

      # CHECK IF TRAIN.SET IS VALID
      if (!"Trainset" %in% class(train.set)) {
        stop("[", class(self)[1], "][FATAL] Train set parameter must be ",
              "defined as 'Trainset' type. Aborting...")
      }

      if (!"TrainFunction" %in% class(train.function)) {
        stop("[", class(self)[1], "][FATAL] Train function parameter must be ",
              "defined as 'TrainFunction' type. Aborting...")
      }

      if (any(is.null(num.clusters), !is.numeric(num.clusters),
               !is.vector(num.clusters))) {
        message("[", class(self)[1], "][WARNING] Number of clusters not set ",
                "(must be numeric or vector). Using all clusters")
        num.clusters <- c(1:train.set$getNumClusters())
      } else {
        if (all(is.numeric(num.clusters), num.clusters > train.set$getNumClusters())) {
          message("[", class(self)[1], "][WARNING] Number of clusters ",
                  "is higher than number of existing clusters. ",
                  "Using all clusters")
          num.clusters <- c(1:train.set$getNumClusters())
        } else num.clusters <- c(1:num.clusters)
      }

      # VERIFY IF EX.CLASSIFIERS PARAMETER IS DEFINED (AND VALID)
      if (all(is.character(ex.classifiers), length(ex.classifiers) > 0)) {
        usedModels <- ex.classifiers
      } else { usedModels <- private$availableModels$name }

      # VERIFY IF IG.CLASSIFIERS PARAMETER IS DEFINED (AND VALID)
      if (all(is.character(ig.classifiers), length(ig.classifiers) > 0)) {
        message("[", class(self)[1], "][INFO] Ignoring '",
                length(ig.classifiers), "' M.L models")
        usedModels <- setdiff(usedModels, ig.classifiers)
        message("[", class(self)[1], "][INFO] Still '", length(usedModels),
                "' M.L models available")
      }

      # VERIFY IF METRIC PARAMETER IS DEFINED (AND VALID)
      if (!all(is.character(metrics), length(metrics) > 0)) {
        stop("[", class(self)[1], "][FATAL] Invalid values of metrics")
      }

      message("[", class(self)[1], "][INFO] Making parallel socket cluster with ",
              private$cluster.conf$cores, " cores")

      private$cluster.obj <- parallel::makeCluster(private$cluster.conf$cores,
                                                    type = private$cluster.conf$socket,
                                                    outfile = private$cluster.conf$outfile,
                                                    useXDR = private$cluster.conf$xdr)

      cluster.models <- lapply(metrics, function(x) vector(mode = "list",
                                                           length = train.set$getNumClusters()))
      names(cluster.models) <- metrics
      available.models <- private$availableModels[(private$availableModels$name %in% usedModels), ]
      # START TRAINING PROCESS
      for (row in 1:nrow(available.models)) {
        current.model <- available.models[row, ]
        message("[", class(self)[1], "][INFO][", current.model$name, "]",
                " ***********************************************************************")
        message("[", class(self)[1], "][INFO][", current.model$name, "] ",
                "'Model [", row, "-", nrow(available.models), "]': Start training")
        message("[", class(self)[1], "][INFO][", current.model$name, "]",
                " ***********************************************************************")
        loaded.packages <- FALSE

        for (current.metric in metrics) {
          message("[", class(self)[1], "][INFO][", current.model$name, "] ",
                  "----------------------------------------------------------------------")
          message("[", class(self)[1], "][INFO][", current.model$name, "] ",
                  "'Metric [", which(current.metric == metrics), "-", length(metrics), "]': ",
                  "Training model for metric '", current.metric, "'")

          for (current.cluster in 1:train.set$getNumClusters()) {
            # DELETE IMCOMPATIBLE MODELS
            # if ( abs(mean(cor(train.set$getFeatureValues(current.cluster),
            #                   as.numeric(train.set$getClassValues()) ),
            #               na.rm = TRUE) ) < 0.3 &
            #      grepl("Linea[l|r]|Discriminant",
            #            paste(current.model$description,
            #                  current.model$family, sep = " "))) {  ##CORRELATION
            #   message("[", class(self)[1], "][WARNING] Correlated Data (< 0.3).",
            #           " Incompatible M.L. model '", current.model$name,
            #           "' on the cluster '", current.cluster, "'")
            #   next
            # }
            model.path <- file.path(private$path, current.metric,
                                     paste0("C[", current.cluster, "-",
                                            train.set$getNumClusters(), "]"))
            executed.models <- ExecutedModels$new(model.path)

            if (!executed.models$exist(current.model$name)) {

              message("[", class(self)[1], "][INFO][", current.model$name, "]",
                      " ----------------------------------------------------------------------")
              message("[", class(self)[1], "][INFO][", current.model$name, "]",
                      " Training on cluster 'C[", current.cluster, "-", train.set$getNumClusters(), "]'")
              message("[", class(self)[1], "][INFO][", current.model$name, "]",
                      " ----------------------------------------------------------------------")
              # LOAD REQUIRED PACKAGES
              if (!loaded.packages) {
                if (!is.null(current.model$model.libs) &&
                     !is.na(current.model$model.libs) &&
                     !current.model$model.libs %in% "NA") {
                  len.init.packages <- length(.packages())
                  len.init.DLLs <- length(.dynLibs())
                  message("[", class(self)[1], "][INFO][", current.model$name, "] ",
                          "Loading required packages...")
                  private$loadPackages(current.model$model.libs)
                }
                loaded.packages <- TRUE
              }

              ifelse(isTRUE(current.model$prob),
                     train.function$create(UseProbability$new(),
                                           search.method = "random",
                                           class.probs = TRUE),
                     train.function$create(NoProbability$new(),
                                           search.method = "random",
                                           class.probs = FALSE)
              )

              model.instances <- train.set$getInstances(current.cluster)
              model.recipe <- DefaultModelFit$new(model.instances,
                                                  train.set$getClassName())$createRecipe()
              model.type <- Model$new(dir = model.path, model = current.model)
              model.type$train(train.set = model.instances, fitting = model.recipe,
                               trFunction = train.function, metric = current.metric)
              if (model.type$isTrained()) {
                message("[", class(self)[1], "][INFO][", current.model$name, "] ",
                        "Model has been succesfully trained")
                executed.models$add(model.type, keep.best = !isTRUE(saveAllModels))
                executed.models$save()
              } else {
                message("[", class(self)[1], "][WARNING] Unable to train model '",
                        current.model$name, "'. Skipping...")
                executed.models$add(model.type, keep.best = !isTRUE(saveAllModels))
                executed.models$save()
              }
            } else {
              message("[", class(self)[1], "][INFO][", current.model$name, "] ",
                      "'Cluster[", current.cluster, "-", train.set$getNumClusters(), "]': ",
                      "Model has been previously trained. Skipping...")
            }
            cluster.models[[current.metric]][[current.cluster]] <- executed.models$getBest()$train
          }
        }
        # UNLOAD REQUIRED PACKAGES
        if (loaded.packages && !is.null(current.model$model.libs) &&
             !is.na(current.model$model.libs) &&
             !current.model$model.libs %in% "NA") {
          message("[", class(self)[1], "][INFO][", current.model$name, "] ",
                  "Detaching required packages...")
          private$unloadPackages(len.init.packages, len.init.DLLs)
        }
      }

      message("[", class(self)[1], "][INFO] ----------------------------------",
              "---------------------")
      message("[", class(self)[1], "][INFO] Finished")
      message("[", class(self)[1], "][INFO] ----------------------------------",
              "---------------------")

      if (!is.null(private$cluster.obj)) {
        parallel::stopCluster(private$cluster.obj)
        private$cluster.obj <- NULL
      }

      TrainOutput$new(models = cluster.models,
                      class.values = train.set$getClassValues(),
                      positive.class = train.set$getPositiveClass())
    },
    #'
    #' @description <<description>>
    #'
    #' @param train.output <<description>>
    #' @param subset <<description>>
    #' @param voting.types <<description>>
    #' @param positive.class <<description>>
    #'
    #' @return <<description>>
    #'
    classify = function(train.output, subset, voting.types, positive.class = NULL) {

      if (!inherits(train.output, "TrainOutput"))
        stop("[", class(self)[1], "][FATAL] Train output parameter must be ",
             "defined as 'TrainOutput' type. Aborting...")

      if (!inherits(subset, c("Subset", "HDSubset")))
        stop("[", class(self)[1], "][FATAL] Subset parameter must be defined as ",
             "'Subset' or 'HDSubset' type. Aborting...")

      if (missing(voting.types)) {
        stop("[", class(self)[1], "][FATAL] Voting types parameter are missing.",
             "Aborting...")
      }

      if (!is.list(voting.types) || !is.vector(voting.types)) {
        voting.types <- list(voting.types)
      }

      if (!all(sapply(voting.types,  function(x) {
        inherits(x, c("SingleVoting", "CombinedVoting"))
      }))) {
        stop("[", class(self)[1], "][FATAL] Voting Schemes parameter must be ",
             "defined as 'SingleVoting' or 'CombinedVoting' types. Aborting...")
      }

      class.values <- unique(train.output$getClassValues())
      if (is.factor(class.values)) class.values <- levels(class.values)

      if (subset$isBlinded()) {
        if (is.null(positive.class)) {
          message("[", class(self)[1], "][WARNING] Positive class is not set. ",
                  "Asuming positive class value used during training stage '",
                  train.output$getPositiveClass(), "'")
          positive.class <- train.output$getPositiveClass()
        } else {
          if (!(positive.class %in% class.values)) {
            stop("[", class(self)[1], "][FATAL] Positive class has ",
                 "not being defined during training stage. Aborting...")
          }
        }
      } else {
        if (is.null(positive.class)) {
          message("[", class(self)[1], "][WARNING] Positive class not set. ",
                  "Asuming positive class value used during training stage '",
                  train.output$getPositiveClass(), "'")
          positive.class <- train.output$getPositiveClass()
        } else {
          if (!(positive.class %in% class.values)) {
            message("[", class(self)[1], "][WARNING] Positive class value is ",
                    "invalid. Must be [", paste0(class.values, collapse = ", "), "].",
                    " Assuming positive class used during training stage (",
                    train.output$getPositiveClass(), ")")
            positive.class <- train.output$getPositiveClass()
          }
        }
      }


      exec.metrics <- unique(unlist(sapply(voting.types, function(voting) {
        voting$getMetrics() })))

      cluster.predictions <- list()
      final.votings <- list()
      final.models <- list()

      for (metric in exec.metrics) {
        if (!metric %in% train.output$getMetrics())
        {
          message("[", class(self)[1], "][WARNING] Models were not trained for '",
                  metric, "' metric. Executing next metric...")
          next
        }
        predictions <- ClusterPredictions$new(class.values = class.values,
                                               positive.class = positive.class)
        num.clusters <- length(train.output$getModels(metric))
        message("[", class(self)[1], "][INFO] ********************************",
                "***********************")
        message("[", class(self)[1], "][INFO] Executing predictions for ",
                metric, " metric")
        message("[", class(self)[1], "][INFO] ********************************",
                "***********************")
        for (cluster in seq_len(num.clusters)) {
          message("[", class(self)[1], "][INFO] ------------------------------",
                  "-------------------------")
          message("[", class(self)[1], "][INFO] Computing cluster '",
                  cluster, "' of '", num.clusters, "'")
          pred <- Prediction$new(model = train.output$getModels(metric)[[cluster]],
                                 feature.id = subset$getID())
          final.models <- append(final.models, list(train.output$getModels(metric)))
          names(final.models)[[length(final.models)]] <- metric

          iterator <- subset$getIterator(chunk.size = 10000)
          while (!iterator$isLast()) {
            instances <- iterator$getNext()
            pred$execute(instances, class.values, positive.class)
          }
          iterator$finalize()
          rm(iterator)
          predictions$add(pred)
        }
        message("[", class(self)[1], "][INFO] --------------------------------",
                "-----------------------")
        cluster.predictions[[metric]] <- predictions
      }


      for (voting.type in voting.types) {
        valid.metrics <- intersect(voting.type$getMetrics(),
                                    names(cluster.predictions))
        if (length(valid.metrics) == 0) {
          message("[DDMCS][INFO] Metrics for '", voting.type$getName(), "' were ",
                  "not computed. Ignoring voting type...")
          next
        }
        voting.name <- class(voting.type)[1]
        message("[", class(self)[1], "][INFO] ********************************",
                "***********************")
        message("[DDMCS][INFO] Computing final prediction values using '",
                voting.type$getName(), "' schemes")
        message("[", class(self)[1], "][INFO] ********************************",
                "***********************")
        voting.result <- voting.type$execute(cluster.predictions[valid.metrics])

        final.votings[[voting.name]] <- append(final.votings[[voting.name]],
                                               voting.result)
      }

      if (length(final.votings) == 0) {
        message("[", class(self)[1], "][ERROR] No voting system could be ",
                "executed for the indicated metrics. Task not performed")
        NULL
      } else {

        message("[", class(self)[1], "][INFO] ----------------------------------",
                "---------------------")
        message("[", class(self)[1], "][INFO] Finished")
        message("[", class(self)[1], "][INFO] ----------------------------------",
                "---------------------")

        ClassificationOutput$new(voting.schemes = final.votings, models = final.models)
      }
    },
    #'
    #' @description <<description>>
    #'
    #' @return <<description>>
    #'
    getAvailableModels = function() { private$availableModels[, c(1, 2)] }
  ),
  private = list(
    loadAvailableModels = function() {
      model.list <- caret::getModelInfo()

      if (is.null(model.list)) {
        stop("[", class(self)[1], "][FALTAL] Models not found in caret library. ",
             "Aborting...")
      }
      model.names <- names(model.list)

      models <- do.call(rbind, apply(t(model.names), 2, function(name, modelList) {
        if (!name %in% c("null") &&
            all(modelList[[name]]$type %in% "Classification")) {
          data.frame(name = name, description = modelList[[name]]$label,
                      family = base::trimws(modelList[[name]]$tags[1]),
                      library = I(list(modelList[[name]]$library)),
                      prob = (!is.null(modelList[[name]]$prob) &&
                              length(grep("response", deparse(modelList[[name]]$prob))) == 0),
                      stringsAsFactors = FALSE)
        }
      }, modelList = model.list))

      message("[", class(self)[1], "][INFO] ", nrow(models),
              " classifiers has been succesfully loaded")
      models <- with(models, models[order(models$family, models$name), ])
      models
    },
    loadPackages = function(pkgName) {
      pkgName <- pkgName[pkgName %in% available.packages()[, 1] ]
      if (length(pkgName) > 0) {
        new.packages <- pkgName[!(pkgName %in% installed.packages()[, "Package"])]
        if (length(new.packages) > 0) {
          message("[", class(self)[1], "][INFO]", length(new.packages),
                  " packages needed. Installing packages ...")
          suppressMessages(install.packages(new.packages,
                                             repos = "https://ftp.cixug.es/CRAN/",
                                             dependencies = TRUE,
                                             quiet = TRUE, verbose = FALSE))
        }
        lapply(pkgName, function(pkg) {
          if (!pkg %in% devtools::loaded_packages()) {
            suppressMessages(library(pkg, character.only = TRUE, warn.conflicts = FALSE,
                                     verbose = FALSE, quietly = TRUE,
                                     attach.required = TRUE))
          }
        })
      } else {
        message("[", class(self)[1], "][ERROR] Packages are not available ",
                "on CRAN...")
      }
    },
    unloadPackages = function(len.init.packages, len.init.DLLs) {
      pkgs <- paste0("package:", head(x = .packages(), n = length(.packages()) - len.init.packages))
      if (length(head(x = .packages(), n = length(.packages()) - len.init.packages)) > 0) {
        message("[", class(self)[1], "][INFO] Package to detach: ", paste(pkgs, collapse = " "))
        for (p in pkgs) {
          detach(p, unload = T, character.only = TRUE, force = F)
        }
      } else {
        # message("[", class(self)[1], "][INFO] There are not packages to detach")
      }

      pkglibs <- tail(x = .dynLibs(), n = length(.dynLibs()) - len.init.DLLs)
      if (length(pkglibs) > 0) {
        # message("[", class(self)[1], "][INFO] Dlls to detach: ", paste(pkglibs, collapse = " "))
        for (lib in pkglibs) {
          dyn.unload(lib[["path"]])
        }
        libs <- .dynLibs()
        .dynLibs(libs[!(libs %in% pkglibs)])
      } else {
        # message("[", class(self)[1], "][INFO] There are not DLLs to unload\n")
      }
    },
    cluster.conf = NULL,
    cluster.obj = NULL,
    availableModels = NULL,
    path = NULL
  )
)
