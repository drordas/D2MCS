#' @title <<tittle>>
#'
#' @description MeasureFunction
#'
#' @docType class
#'
#' @format NULL
#'
#' @details <<details>
#'
#' @seealso \code{\link{MeasureFunction}}
#'
#' @keywords NULL
#'
#' @import R6
#'
#' @export MeasureFunction

MeasureFunction <- R6::R6Class(
  classname = "MeasureFunction",
  portable = TRUE,
  public = list(
    #'
    #' @description <<description>>
    #'
    #' @param performance <<description>>
    #'
    initialize = function(performance = NULL) {
      if (!is.null(performance) && !inherits(performance, c("MinResult", "ConfMatrix")))
        stop("[", class(self)[1], "][FATAL] Performance parameter must be ",
             "defined as 'MinResult' or 'ConfMatrix' type. Aborting...")

      private$performance <- performance
    },
    #'
    #' @description <<description>>
    #'
    #' @param performance.output <<description>>
    #'
    #' @return <<description>>
    #'
    compute = function(performance.output = NULL) {
      stop("[", class(self)[1], "][FATAL] Class is abstract. ",
           "Method should be defined in inherited class. Aborting...")
    }
  ),
  private = list(
    performance = NULL
  )
)
