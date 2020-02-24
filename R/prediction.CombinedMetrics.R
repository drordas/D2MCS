#' @title <<tittle>>
#'
#' @description CombinedMetrics
#'
#' @docType class
#'
#' @format NULL
#'
#' @details <<details>
#'
#' @seealso \code{\link{CombinedVoting}}
#'
#' @keywords NULL
#'
#' @import R6
#'
#' @export CombinedMetrics

CombinedMetrics <- R6::R6Class(
  classname = "CombinedMetrics",
  portable = TRUE,
  public = list(
    #'
    #' @description <<description>>
    #'
    #' @param required.metrics <<description>>
    #'
    initialize = function(required.metrics) {
      if (is.null(required.metrics) || !is.character(required.metrics) || length(required.metrics) < 2) {
        stop("[", class(self)[1], "][FATAL] Required.metrics parameter must be ",
             "defined as 'character' type. Aborting...")
      }
      private$required.metrics <- required.metrics
    },
    #'
    #' @description <<description>>
    #'
    #' @return <<description>>
    #'
    getRequiredMetrics = function() { private$required.metrics },
    #'
    #' @description <<description>>
    #'
    #' @param raw.pred <<description>>
    #' @param prob.pred <<description>>
    #' @param positive.class <<description>>
    #' @param negative.class <<description>>
    #'
    #' @return <<description>>
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
