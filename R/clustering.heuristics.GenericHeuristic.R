#' @title Abstract Feature Clustering heuristic object.
#'
#' @description Abstract class used as a template to define new customized
#' clustering heuristics.
#'
#' @details The \link{GenericHeuristic} is an archetype class so it cannot be
#' instantiated.
#'
#' @seealso \code{\link{Dataset}}
#'
#' @keywords cluster manip
#'
#' @import R6
#'
#' @export GenericHeuristic

GenericHeuristic <- R6::R6Class(
  classname = "GenericHeuristic",
  portable = TRUE,
  public = list(
    #'
    #' @description Empty function used to initialize the object arguments in
    #' runtime.
    #'
    initialize = function() { },
    #'
    #' @description Function used to implement the clustering heuristic.
    #'
    #' @param col1 A \link{numeric} vector or matrix required to perform the
    #' clustering operation.
    #' @param col2 A \link{numeric} vector or matrix to perform the clustering
    #' operation.
    #' @param column.names An optional \link{character} vector with the names of
    #' both columns
    #' @param ... Further arguments passed down to \code{heuristic} function.
    #'
    #' @return A \link{numeric} vector of length 1.
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
