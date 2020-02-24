#' @title <<tittle>>
#'
#' @description GenericHeuristic
#'
#' @docType class
#'
#' @format NULL
#'
#' @details <<details>
#'
#' @seealso \code{\link{Dataset}}
#'
#' @keywords NULL
#'
#' @import R6
#'
#' @export GenericHeuristic

GenericHeuristic <- R6::R6Class(
  classname = "GenericHeuristic",
  portable = TRUE,
  public = list(
    #' @description <<description>>
    initialize = function() { },
    #'
    #' @param col1 <<description>>
    #' @param col2 <<description>>
    #' @param column.names <<description>>
    #' @param ... <<description>>
    #'
    #' @description <<description>>
    #'
    #' @return <<return>>
    #'
    heuristic = function(col1, col2, column.names = NULL, ...) {
      stop("[", class(self)[1], "][FATAL] Class is abstract. ",
           "Method should be defined in inherited class. Aborting...")
    }
  ),
  private = list(
    isBinary = function(column) {
      length(levels(factor(column))) == 2
    }
  )
)
