#' @title Combined metric strategy to minimize FN errors.
#'
#' @description Calculates if the positive class is the predicted one in any of
#' the metrics, otherwise, the instance is not considered to have the positive
#' class associated.
#'
#' @seealso \code{\link{CombinedMetrics}}
#'
#' @keywords classif math
#'
#' @import R6
#'
#' @export MinimizeFN

MinimizeFN <- R6::R6Class(
  classname = "MinimizeFN",
  portable = TRUE,
  inherit = CombinedMetrics,
  public = list(
    #'
    #' @description Method for initializing the object arguments during runtime.
    #'
    #' @param required.metrics A \link{character} vector of length 1 with the
    #' name of the required metrics.
    #'
    initialize = function(required.metrics = c("MCC", "PPV")) {
      if (any(is.null(required.metrics),
              !is.character(required.metrics),
              length(required.metrics) < 2)) {
        stop("[", class(self)[1], "][FATAL] Invalid values of required.metrics. Aborting...")
      }
      super$initialize(required.metrics = required.metrics)
    },
    #'
    #' @description Function to obtain the final prediction based on different
    #' metrics.
    #'
    #' @param raw.pred A \link{character} list of length greater than 2 with the
    #' class value of the predictions made by the metrics.
    #' @param prob.pred A \link{numeric} list of length greater than 2 with the
    #' probability of the predictions made by the metrics.
    #' @param positive.class A \link{character} with the value of the positive
    #' class.
    #' @param negative.class A \link{character} with the value of the negative
    #' class.
    #'
    #' @return A \link{logical} value indicating if the instance is predicted
    #' as positive class or not.
    #'
    getFinalPrediction = function(raw.pred, prob.pred, positive.class, negative.class) {
      if (is.null(raw.pred) || !is.list(raw.pred)) {
        stop("[", class(self)[1], "][FATAL] Raw.pred parameter must be defined ",
             "as 'list' type. Aborting...")
      }
      if (!all(self$getRequiredMetrics() %in% names(raw.pred))) {
        stop("[", class(self)[1], "][FATAL] Raw.pred parameter must have required metrics. ",
             paste(self$getRequiredMetrics(), collapse = " "), ". Aborting...")
      }

      if (is.null(prob.pred) || !is.list(prob.pred)) {
        stop("[", class(self)[1], "][FATAL] Prob.pred parameter must be defined ",
             "as 'list' type. Aborting...")
      }
      if (!all(self$getRequiredMetrics() %in% names(prob.pred))) {
        stop("[", class(self)[1], "][FATAL] Prob.pred parameter must have required metrics. ",
             paste(self$getRequiredMetrics(), collapse = " "), ". Aborting...")
      }

      if (is.null(positive.class) || (!is.character(positive.class) && !is.numeric(positive.class))) {
        stop("[", class(self)[1], "][FATAL] Positive class parameter must be defined. Aborting...")
      }
      if (is.null(negative.class) || (!is.character(negative.class) && !is.numeric(negative.class))) {
        stop("[", class(self)[1], "][FATAL] Negative class parameter must be defined. Aborting...")
      }

      ifelse(all(raw.pred[self$getRequiredMetrics()] == negative.class),
             FALSE,
             TRUE)
    }
  )
)
