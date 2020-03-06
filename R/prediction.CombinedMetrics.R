#' @title Abstract class to compute the class prediction based on combination
#' between metrics.
#'
#' @description Abstract class used as a template to define new customized
#' strategies to combine the class predictions made by different metrics.
#'
#' @docType class
#'
#' @seealso \code{\link{CombinedVoting}}
#'
#' @keywords models methods math
#'
#' @import R6
#'
#' @export CombinedMetrics

CombinedMetrics <- R6::R6Class(
  classname = "CombinedMetrics",
  portable = TRUE,
  public = list(
    #'
    #' @description Method for initializing the object arguments during runtime.
    #'
    #' @param required.metrics A \code{\link{character}} vector of length greater than 2 with the name of the required metrics.
    #'
    initialize = function(required.metrics) {
      if (is.null(required.metrics) || !is.character(required.metrics) || length(required.metrics) < 2) {
        stop("[", class(self)[1], "][FATAL] The required.metrics parameter must be ",
             "defined as 'character' type. Aborting...")
      }
      private$required.metrics <- required.metrics
    },
    #'
    #' @description The function returns the required metrics that will participate in the combined metric process.
    #'
    #' @return A \code{\link{character}} vector of length greater than 2 with the name of the required metrics.
    #'
    getRequiredMetrics = function() { private$required.metrics },
    #'
    #' @description Function used to implement the strategy to obtain the final prediction based on different metrics.
    #'
    #' @param raw.pred A \code{\link{character}} list of length greater than 2 with the class value of the predictions made by the metrics.
    #' @param prob.pred A \code{\link{numeric}} list of length greater than 2 with the probability of the predictions made by the metrics.
    #' @param positive.class A \code{\link{character}} with the value of the positive class.
    #' @param negative.class A \code{\link{character}} with the value of the negative class.
    #'
    #' @return A \link{logical} value indicating if the instance is predicted as positive class or not.
    #'
    getFinalPrediction = function(raw.pred, prob.pred, positive.class, negative.class) {
      stop("[", class(self)[1], "][FATAL] Class is abstract. ",
           "Method should be defined in inherited class. Aborting...")
    }
  ),
  private = list(
    required.metrics = c()
  )
)
