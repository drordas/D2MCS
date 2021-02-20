#' @title Archetype to define customized measures.
#'
#' @description Abstract class used as a template to define new M.L. performance
#' measures.
#'
#' @details The \code{\link{GenericHeuristic}} is an full-abstract class so it cannot
#' be instantiated. To ensure the proper operation, \code{compute} method is
#' automatically invoke by \code{\link{DDMCS}} framework when needed.
#'
#' @seealso \code{\link{MeasureFunction}}
#'
#' @keywords classif math
#'
#' @import R6
#'
#' @export MeasureFunction

MeasureFunction <- R6::R6Class(
  classname = "MeasureFunction",
  portable = TRUE,
  public = list(
    #'
    #' @description Method for initializing the object arguments during runtime.
    #'
    #' @param performance An optional \code{\link{ConfMatrix}} parameter to
    #' define the type of object used to compute the measure.
    #'
    initialize = function(performance = NULL) {
      if (!is.null(performance) && !inherits(performance, c("MinResult", "ConfMatrix")))
        stop("[", class(self)[1], "][FATAL] Performance parameter must be ",
             "defined as 'MinResult' or 'ConfMatrix' type. Aborting...")

      private$performance <- performance
    },
    #'
    #' @description The function implements the metric used to measure the
    #' performance achieved by the M.L. model.
    #'
    #' @param performance.output An optional \code{\link{ConfMatrix}} parameter
    #' to define the type of object used to compute the measure.
    #'
    #' @details This function is automatically invoke by the \code{\link{DDMCS}}
    #' framework.
    #'
    #' @return A \link{numeric} vector of size 1 or \link{NULL} if an error
    #' occurred.
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
