#' @title <<tittle>>
#'
#' @description SummaryFunction
#'
#' @docType class
#'
#' @format NULL
#'
#' @details <<details>
#'
#' @seealso \code{\link{NoProbability}}
#'
#' @keywords NULL
#'
#' @import R6
#'
#' @export SummaryFunction

SummaryFunction <- R6::R6Class(
  classname = "SummaryFunction",
  portable = TRUE,
  public = list(
    # '
    #' @description <<description>>
    #'
    #' @param measures
    #'
    initialize = function(measures) {
      if (is.null(measures))
        stop("[", class(self)[1], "][FATAL] Measures were not defined. Aborting...")
      private$measures <- measures
    },
    #'
    #' @description <<description>>
    #'
    #' @return <<description>>
    #'
    execute = function() {
      stop("[", class(self)[1], "][FATAL] Class is abstract. ",
           "Method should be defined in inherited class. Aborting...")
    },
    #'
    #' @description <<description>>
    #'
    #' @return <<description>>
    #'
    getMeasures = function() {
      private$measures
    }
  ),
  private = list(
    measures = NULL
  )
)
