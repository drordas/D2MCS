#' @title Methodology to obtain the combination of the probability of different metrics
#'
#' @description Calculates the mean of the probabilities of the different metrics.
#'
#' @seealso \code{\link{Methodology}}
#'
#' @keywords models methods math
#'
#' @import R6
#'
#' @export ProbBasedMethodology

ProbBasedMethodology <- R6::R6Class(
  classname = "ProbBasedMethodology",
  portable = TRUE,
  inherit = Methodology,
  public = list(
    #'
    #' @description Method for initializing the object arguments during runtime.
    #'
    #' @param required.metrics A \code{\link{character}} vector of length greater than 2 with the name of the required metrics.
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
    #' @description Function to compute the probability of the final prediction based on different metrics.
    #'
    #' @param raw.pred A \code{\link{character}} list of length greater than 2 with the class value of the predictions made by the metrics.
    #' @param prob.pred A \code{\link{numeric}} list of length greater than 2 with the probability of the predictions made by the metrics.
    #' @param positive.class A \code{\link{character}} with the value of the positive class.
    #' @param negative.class A \code{\link{character}} with the value of the negative class.
    #'
    #' @return A \link{numeric} value indicating the probability of the instance is predicted as positive class.
    #'
    compute = function(raw.pred, prob.pred, positive.class, negative.class) {
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

      Reduce(prod, prob.pred[which(names(prob.pred) %in% self$getRequiredMetrics())])
    }
  )
)
