#' @title Abstract class to compute the probability prediction based on
#' combination between metrics.
#'
#' @description Abstract class used as a template to define new customized
#' strategies to combine the probability predictions made by different metrics.
#'
#' @seealso \code{\link{ProbBasedMethodology}}
#'
#' @keywords math misc
#'
#' @import R6
#'
#' @export Methodology

Methodology <- R6::R6Class(
  classname = "Methodology",
  portable = TRUE,
  public = list(
    #'
    #' @description Method for initializing the object arguments during runtime.
    #'
    #' @param required.metrics A \code{\link{character}} vector of length greater
    #' than 2 with the name of the required metrics.
    #'
    initialize = function(required.metrics) {
      if (is.null(required.metrics) || !is.character(required.metrics) || length(required.metrics) < 2) {
        stop("[", class(self)[1], "][FATAL] Required.metrics parameter must be ",
             "defined as 'character' type. Aborting...")
      }
      private$required.metrics <- required.metrics
    },
    #'
    #' @description The function returns the required metrics that will
    #' participate in the methodology to compute a metric based on all of them.
    #'
    #' @return A \code{\link{character}} vector of length greater than 2 with
    #' the name of the required metrics.
    #'
    getRequiredMetrics = function() { private$required.metrics },
    #'
    #' @description Function to compute the probability of the final prediction
    #' based on different metrics.
    #'
    #' @param raw.pred A \code{\link{character}} list of length greater than 2
    #' with the class value of the predictions made by the metrics.
    #' @param prob.pred A \code{\link{numeric}} list of length greater than 2
    #' with the probability of the predictions made by the metrics.
    #' @param positive.class A \code{\link{character}} with the value of the
    #' positive class.
    #' @param negative.class A \code{\link{character}} with the value of the
    #' negative class.
    #'
    #' @return A \link{numeric} value indicating the probability of the instance
    #' is predicted as positive class.
    #'
    compute = function(raw.pred, prob.pred, positive.class, negative.class) {
      stop("[", class(self)[1], "][FATAL] Class is abstract. ",
           "Method should be defined in inherited class. Aborting...")
    }
  ),
  private = list(
    required.metrics = c()
  )
)
