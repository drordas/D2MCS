#' @title Manages the prediction computed for a specific model.
#'
#' @description Allows to obtain predictions from the data provided using a pre-trained model.
#'
#' @seealso \code{\link{ClusterPredictions}}
#'
#' @keywords internal math misc
#'
#' @import R6
#' @importFrom devtools loaded_packages
#'
#' @export Prediction

Prediction <- R6::R6Class(
  classname = "Prediction",
  portable = TRUE,
  public = list(
    #'
    #' @description Method for initializing the object arguments during runtime.
    #'
    #' @param model A \link{list} containing the information of the
    #' trained model composed of five elements: "model.name", "exec.time",
    #' "model.performance", "model.data" and "model.libs".
    #' @param feature.id A \link{character} value containing the column name
    #' used as identifier.
    #'
    initialize = function(model, feature.id = NULL) {
      if (!inherits(model, "list") || length(model) != 5) {
        stop("[", class(self)[1], "][FATAL] Model parameter must be defined as a ",
             "list of five elements. Aborting...")
      }

      private$model <- model
      private$feature.id <- feature.id
      private$results <- list(id = c(), raw = data.frame(), prob = data.frame())
      private$loadPackages(private$model$model.libs)
    },
    #'
    #' @description Calculates predictions of the values passed by parameters
    #' using the corresponding model.
    #'
    #' @param pred.values A \link{data.frame} containing the values to predict.
    #' @param class.values A \link{vector} containing the class values.
    #' @param positive.class A \link{character} value containing the positive
    #' class.
    #'
    execute = function(pred.values, class.values, positive.class) {
      if (!inherits(pred.values, "data.frame")) {
        stop("[", class(self)[1], "][FATAL] Prediction values parameter must be ",
             "defined as 'data.frame' type. Aborting...")
      }

      if (all(!is.null(private$feature.id), length(private$feature.id) > 0)) {

        private$results$id <- c(private$results$id,
                                as.character(pred.values[, private$feature.id]))
        pred.values[, -which(names(pred.values) == private$feature.id)]
      }

      if (isTRUE(private$model$model.data$control$classProbs)) {

        prob.aux <- predict(object = private$model$model.data,
                            newdata = pred.values, type = "prob")
        private$results$prob <- rbind(private$results$prob, prob.aux)
        names(private$results$prob) <- class.values

        raw.aux <- factor(apply(prob.aux, 1, function(row, names, pclass, cutoff) {
          pos <- which(row > cutoff)
          ifelse(length(pos) == 1, names[pos], pclass)
        }, names = class.values, pclass = positive.class, cutoff = 0.5),
        levels = class.values)
        relevel(raw.aux, ref = as.character(positive.class))

        private$results$raw <- rbind(private$results$raw, data.frame(raw.aux))

      } else {
        message("[", class(self)[1], "][WARNING] Model '", private$model$model.name,
                "' is not able to compute a-posteriori probabilities")

        raw.aux <- data.frame(predict(object = private$model$model.data,
                                      newdata = pred.values,
                                      type = "raw"))

        private$results$raw <- rbind(private$results$raw, raw.aux)

        if (is.null(private$results$prob)) {
          names(private$results$prob) <- class.values
        }

        prob.aux <- do.call(rbind, apply(raw.aux, 1, function(row, class.values) {
          m <- matrix(0, nrow = 1, ncol = length(class.values))
          m[ which(row == class.values) ] <- 1
          data.frame(m)
        }, class.values = names(private$results$prob)))

        private$results$prob <- rbind(private$results$prob, prob.aux)
      }
    },
    #'
    #' @description The function is used to return the prediction values
    #' computed.
    #'
    #' @param type A \link{character} to define which type of predictions
    #' should be returned. If not defined all type of probabilities will be
    #' returned. Conversely if "prob" or "raw" is defined then computed
    #' 'probabilistic' or 'class' values are returned.
    #' @param target A \link{character} defining the value of the positive
    #' class.
    #'
    #' @return A \link{data.frame} with the computed prediction.
    #'
    getPrediction = function(type = NULL, target = NULL) {
      if (is.null(type) || !type %in% c("raw", "prob")) {
        message("[", class(self)[1], "][WARNING] Probability type ",
                "missing or incorrect. Should be 'raw' or 'prob'. ",
                "Assuming 'raw' by default")
        type <- "raw"
      }
      switch (type,
              "prob" = {
                class.names <- names(private$results$prob)
                if (is.null(target) || !(target %in% class.names)) {
                  message("[", class(self)[1], "][WARNING] Target not ",
                          "specified or invalid. Using '",
                          class.names[1], "' as default value")
                  target <- class.names[1]
                }
                ret <- private$results$prob[, as.character(target), drop = FALSE]
              },
              "raw" = { ret <- as.data.frame(private$results$raw) }
      )

      if (length(private$results$id) != nrow(ret)) {
        private$results$id <- as.integer(seq(from = 1, to = nrow(ret), by = 1))
      }

      ret <- as.data.frame(ret, row.names = private$results$id)
      names(ret) <- ifelse(is.null(target), "Predictions", target)
      ret
    },
    #'
    #' @description Gets the model name.
    #'
    #' @return The \link{character} value of model value.
    #'
    getModelName = function() { private$model$model.name },
    #'
    #' @description Gets the performance of the model.
    #'
    #' @return The \link{numeric} value of the model's performance.
    #'
    getModelPerformance = function() { private$model$model.performance }
  ),
  private = list(
    results = NULL,
    model = NULL,
    loaded.resources = NULL,
    feature.id = NULL,
    loadPackages = function(pkgName) {
      if (is.list(pkgName)) { pkgName <- unlist(pkgName) }

      new.packages <- pkgName[!(pkgName %in% installed.packages()[, "Package"])]
      if (length(new.packages)) {
        message("[", class(self)[1], "][INFO][", private$model$model.name, "]",
                length(new.packages), "packages needed to execute aplication\n",
                "Installing packages...")
        suppressMessages(install.packages(new.packages,
                                          repos = "https://ftp.cixug.es/CRAN/",
                                          dependencies = TRUE,
                                          quiet = TRUE, verbose = FALSE))
      }
      lapply(pkgName, function(pkg) {
        if (!pkg %in% devtools::loaded_packages()) {
          library(pkg, character.only = TRUE, warn.conflicts = FALSE, quietly = TRUE)
        }
      })
    }
  )
)
