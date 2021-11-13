#
# D2MCS provides a novel framework to able to automatically develop and deploy
# an accurate Multiple Classifier System (MCS) based on the feature-clustering
# distribution achieved from an input dataset. D2MCS was developed focused on
# four main aspects: (i) the ability to determine an effective method to
# evaluate the independence of features, (ii) the identification of the optimal
# number of feature clusters, (iii) the training and tuning of ML models and
# (iv) the execution of voting schemes to combine the outputs of each classifier
# comprising the MCS.
#
# Copyright (C) 2021 Sing Group (University of Vigo)
#
# This program is free software: you can redistribute it and/or modify it under
# the terms of the GNU General Public License as published by the Free Software
# Foundation, either version 3 of the License, or (at your option) any later
# version.
#
# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License along with
# this program. If not, see <https://www.gnu.org/licenses/gpl-3.0.html>

#' @title Handles training of M.L. models
#'
#' @description Allows to manage the executed M.L. models.
#'
#' @seealso \code{\link{Model}}
#'
#' @keywords internal methods error utilities misc
#'
#' @import R6
#'
#' @export ExecutedModels

ExecutedModels <- R6::R6Class(
  classname = "ExecutedModels",
  portable = TRUE,
  public = list(
    #'
    #' @description Method for initializing the object arguments during runtime.
    #'
    #' @param dir.path The location were the executed models will be saved.
    #'
    initialize = function(dir.path) {
      private$dir.path <- gsub("\\/$", "", dir.path)
      if (!file.exists(private$dir.path)) {
        dir.create(private$dir.path, recursive = TRUE)
        private$models <- NULL
        private$best.model <- NULL
      }

      if (!file.exists(file.path(private$dir.path, "executed")) ||
          file.info(file.path(private$dir.path, "executed"))$size <= 0) {
        file.create(file.path(private$dir.path, "executed"))
        private$models <- NULL
        private$best.model <- NULL
      } else {
        private$models <- read.csv(file = file.path(private$dir.path, "executed"),
                                    header = TRUE, stringsAsFactors = FALSE, sep = ",")

        best.perf <- private$models[max(which(private$models$performance == max(private$models$performance))), ]

        if (length(which(best.perf$performance != 0)) != 0) {
          best.path <- file.path(private$dir.path, paste0(best.perf$model, ".rds"))
          if (file.exists(best.path)) {
            private$best.model <- list(model = best.perf$model,
                                       performance = best.perf$performance,
                                       exec.time = best.perf$exec.time,
                                       train = readRDS(best.path)
            )
          } else {
            d2mcs.log(message = paste0("Best model cannot be loaded ", best.path),
                      level = "WARN",
                      className = class(self)[1],
                      methodName = "initialize")
            private$best.model <- NULL
          }
        }
      }
    },
    #'
    #' @description The function is used to obtain the name of the ML model
    #' achieved the best performance during training stage.
    #'
    #' @return A \link{character} vector of length 1 of \link{NULL}
    #' if no ML model have been trained.
    #'
    getNames = function() {
      if (!is.null(private$best.model)) {
        private$models[, "model"]
      } else { NULL }
    },
    #'
    #' @description The function is responsible of returning the model achieving
    #' the best performance value during training stage.
    #'
    #' @return A \code{\link{Model}} object.
    #'
    getBest = function() {
      if (!is.null(private$best.model)) {
        private$best.model
      } else {
        d2mcs.log(message = "Best model not found",
                  level = "WARN",
                  className = class(self)[1],
                  methodName = "getBest")
        NULL
      }
    },
    #'
    #' @description The function inserts a new model to the list of executed
    #' models.
    #'
    #' @param model A previously trained model (in \code{\link{Model}} object).
    #' @param keep.best A \link{logical} value to define the saving operation.
    #' If \link{TRUE} only saves the best model, otherwise all executed models
    #' are saved.
    #'
    add = function(model, keep.best = TRUE) {
      if (!inherits(model, "Model")) {
        d2mcs.log(message = paste0("Model parameter must be defined ",
                                   "as 'Model' type. Model not inserted. ",
                                   "Task not performed"),
                  level = "ERROR",
                  className = class(self)[1],
                  methodName = "add")
      } else {

        private$models <- rbind(private$models,
                                data.frame(model = model$getName(),
                                           performance = model$getPerformance(),
                                           exec.time = model$getExecutionTime(),
                                           row.names = NULL))

        if (isTRUE(keep.best)) { # SAVE ONLY BEST MODELS. REMOVE WORST

          if (is.null(private$best.model) || # IS BEST MODEL
              model$getPerformance() > private$best.model$performance ||
              isTRUE(all.equal.numeric(model$getPerformance(), private$best.model$performance))) {

            if (!is.null(private$best.model)) {
              d2mcs.log(message = paste0("Best model found. Replacing '",
                                         private$best.model$model, "' with '",
                                         model$getName(), "'"),
                        level = "INFO",
                        className = class(self)[1],
                        methodName = "add")
              self$delete(private$best.model$model)
            }
            private$best.model <- list(model = model$getName(),
                                       performance = model$getPerformance(),
                                       exec.time = model$getExecutionTime(),
                                       train = model$getTrainedModel())
            model$save()
          }
        } else {
          model$save()
        }
      }
    },
    #'
    #' @description The function is used to discern if a specific model has been
    #' executed previously.
    #'
    #' @param model.name A \link{character} vector with the name of the
    #' model to check for existence.
    #'
    #' @return A \link{logical} value. \link{TRUE} if the model exists
    #' and \link{FALSE} otherwise.
    #'
    exist = function(model.name) {
      if (!is.character(model.name) || is.null(private$models$model)) {
        FALSE
      } else { model.name %in% (private$models$model) }
    },
    #'
    #' @description The function is used to compute the number of executed ML
    #' models.
    #'
    #' @return A \link{numeric} vector or size 1.
    #'
    size = function() {
      ifelse(is.null(private$models), 0, nrow(private$models))
    },
    #'
    #' @description The function is responsible of saving the information of all
    #' executed models into a hidden file.
    #'
    save = function() {
      if (!is.null(private$models) && nrow(private$models) > 0) {
        write.table(private$models, file = file.path(private$dir.path, "executed"),
                    append = FALSE, sep = ",", row.names = FALSE)
      } else {
        d2mcs.log(message = "File is empty. Task not performed",
                  level = "ERROR",
                  className = class(self)[1],
                  methodName = "save")
      }
    },
    #'
    #' @description The function removes an specific model.
    #'
    #' @param model.name A \link{character} vector with the name of the
    #' model to be removed.
    #'
    delete = function(model.name) {
      if (self$exist(model.name)) {
        object.path <- file.path(private$dir.path, paste0(model.name, ".rds"))
        if (file.exists(object.path)) {
          file.remove(object.path)
        } else {
          d2mcs.log(message = paste0("Cannot delete model. Path for model '",
                                     model.name, "' not found. Task not ",
                                     "performed"),
                    level = "ERROR",
                    className = class(self)[1],
                    methodName = "delete")
        }
      } else {
        d2mcs.log(message = paste0("Cannot delete model. Model '", model.name,
                                   "' has not been executed. Task not ",
                                   "performed"),
                  level = "ERROR",
                  className = class(self)[1],
                  methodName = "delete")
      }
    }
  ),
  private = list(
    models = NULL,
    best.model = NULL,
    dir.path = NULL
  )
)
