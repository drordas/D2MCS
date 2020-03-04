#' @title Handles training of M.L. models
#'
#' @description Allows to manage the executed M.L. models.
#'
#' @docType class
#'
#' @seealso \code{\link{Model}}
#'
#' @keywords internal methods error utilities misc
#'
#' @import R6

ExecutedModels <- R6::R6Class(
  classname = "ExecutedModels",
  portable = TRUE,
  public = list(
    #'
    #' @description Method for initializing the object arguments during runtime.
    #'
    #' @param dir.path The location were the executed models will be saved.
    #'
    #' @return An \code{\link{ExecutedModels}} object.
    #'
    initialize = function(dir.path) {
      private$dir.path <- gsub("\\/$", "", dir.path)
      if (!file.exists(private$dir.path)) {
        dir.create(private$dir.path, recursive = TRUE)
        private$models <- NULL
        private$best.model <- NULL
      }

      if (!file.exists(file.path(private$dir.path, ".executed")) ||
          file.info(file.path(private$dir.path, ".executed"))$size <= 0) {
        file.create(file.path(private$dir.path, ".executed"))
        private$models <- NULL
        private$best.model <- NULL
      } else {
        private$models <- read.csv(file = file.path(private$dir.path, ".executed"),
                                    header = TRUE, stringsAsFactors = FALSE, sep = ",")
        best.perf <- private$models[which.max(private$models$performance), ]

        if (length(which(best.perf$performance != 0)) != 0) {
          best.path <- file.path(private$dir.path, paste0(best.perf$model, ".rds"))
          if (file.exists(best.path)) {
            private$best.model <- list(model = best.perf$model,
                                       performance = best.perf$performance,
                                       exec.time = best.perf$exec.time,
                                       train = readRDS(best.path)
            )
          } else {
            message("[", class(self)[1], "][WARNING] Best model cannot be loaded.")
            private$best.model <- NULL
          }
        }
      }
    },
    #'
    #' @description The function is used to obtain the name of the ML model achieved the best performance during training stage.
    #'
    #' @return A \code{\link{character}} vector of length 1 of \code{\link{NULL}} if no ML model have been trainned.
    #'
    getNames = function() {
      if (!is.null(private$best.model)) {
        private$models[, "model"]
      } else { NULL }
    },
    #'
    #' @description The function is responsible of returning the model achieving the best performance value during training stage.
    #'
    #' @return A \code{\link{Model}} oject.
    #'
    getBest = function() {
      if (!is.null(private$best.model)) {
        private$best.model
      } else {
        message("[", class(self)[1], "][WARNING] Best model not found.")
        NULL
      }
    },
    #'
    #' @description The function inserts a new model to the list of executed models.
    #'
    #' @param model A previously trained model (in \code{\link{Model}} object).
    #' @param keep.best A \code{logical} value to define the saving operation.
    #' If \code{TRUE} only saves the best model, otherwise all executed models are saved.
    #'
    add = function(model, keep.best = TRUE) {
      if (!inherits(model, "Model")) {
        message("[", class(self)[1], "][ERROR] Model parameter must be defined ",
                "as 'Model' type. Model not inserted. Task not performed")
      } else {

        private$models <- rbind(private$models,
                                data.frame(model = model$getName(),
                                           performance = model$getPerformance(),
                                           exec.time = model$getExecutionTime()))

        if (isTRUE(keep.best)) { # SAVE ONLY BEST MODELS. REMOVE WORST

          if (any(is.null(private$best.model), # IS BEST MODEL
                  model$getPerformance() > private$best.model$performance)) {
            if (!is.null(private$best.model)) {
              message("[", class(self)[1], "][INFO] Best model found. Replacing '",
                      private$best.model$model, "' with '",
                      model$getName(), "'")
              self$delete(private$best.model$model)
            }
            private$best.model <- list(model = model$getName(),
                                        performance = model$getPerformance(),
                                        exec.time = model$getExecutionTime(),
                                        train = model$getTrainedModel())
            model$save()
          }
        } else { model$save() }
      }
    },
    #'
    #' @description The function is used to discern if a specific model has been executed previously.
    #'
    #' @param model.name A \code{\łink{character}} vector with the name of the model to check for existence.
    #'
    #' @return A \code{\link{logical}} value. \link{TRUE} if the model exists and \link{FALSE} otherwise.
    #'
    exist = function(model.name) {
      if (!is.character(model.name) || is.null(private$models$model)) {
        FALSE
      } else { model.name %in% (private$models$model) }
    },
    #'
    #' @description The function is used to compute the number of executed ML models.
    #'
    #' @return A \code{\link{numeric}} vector or size 1.
    #'
    size = function() {
      ifelse(is.null(private$models), 0, nrow(private$models))
    },
    #'
    #' @description The function is responsible of saving the information of all executed models into a hidden file.
    #'
    save = function() {
      if (nrow(private$models) > 0) {
        write.table(private$models, file = file.path(private$dir.path, ".executed"),
                    append = FALSE, sep = ",", row.names = FALSE)
      } else {
        message("[", class(self)[1], "][ERROR] File is empty. ",
                "Task not performed")
      }
    },    #'
    #' @description The function removes an specific model.
    #'
    #' @param model.name A \code{\link{character}} vector with the name of the model to be removed.
    #'
    delete = function(model.name) {
      if (self$exist(model.name)) {
        object.path <- file.path(private$dir.path, paste0(model.name, ".rds"))
        if (file.exists(object.path)) {
          file.remove(object.path)
        } else {
          message("[", class(self)[1], "][ERROR] Cannot delete model. ",
                  "Path for model '", model.name, "' not found. Task not performed")
        }
      } else {
        message("[", class(self)[1], "][ERROR] Cannot delete model. ",
                "Model '", model.name, "' has not been executed. Task not performed")
      }
    }
  ),
  private = list(
    models = NULL,
    best.model = NULL,
    dir.path = NULL
  )
)
