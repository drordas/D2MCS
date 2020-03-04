#' @title <<miguel>>
#'
#' @description Methodology
#'
#' @docType class
#'
#' @format NULL
#'
#' @details <<details>
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
    compute = function(raw.pred, prob.pred, positive.class, negative.class) {
      stop("[", class(self)[1], "][FATAL] Class is abstract. ",
           "Method should be defined in inherited class. Aborting...")
    }
  ),
  private = list(
    required.metrics = c()
  )
)
