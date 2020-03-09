#' @title Computes the True Positive Value.
#'
#' @description TP is the number of individuals with a positive condition for which the test result is
#' positive. The value entered here must be non-negative.
#'
#' @seealso \code{\link{MeasureFunction}}, \code{\link{ClassificationOutput}}, \code{\link{ConfMatrix}}
#'
#' @keywords classif math
#'
#' @import R6
#'
#' @export TP

TP <- R6::R6Class(
  classname = "TP",
  inherit = MeasureFunction,
  portable = TRUE,
  public = list(
    #'
    #' @description Method for initializing the object arguments during runtime.
    #'
    #' @param performance.output An optional \code{\link{ConfMatrix}} parameter to define the type of
    #' object used to compute the measure.
    #'
    #' @return An \code{\link{TP}} object.
    #'
    initialize = function(performance.output = NULL) {
      super$initialize(performance.output)
    },
    #'
    #' @description The function computes the \code{TP} achieved by the M.L. model.
    #'
    #' @param performance.output An optional \code{\link{ConfMatrix}} parameter to define
    #' the type of object used as basis to compute the \code{TP} measure.
    #' @details This function is automatically invoque by the \link{ClassificationOutput} object.
    #' @seealso \code{\link{ConfMatrix}}
    #' @return A \code{\link{numeric}} vector of size 1 or \code{\link{NULL}} if an error occured.
    #'
    compute = function(performance.output = NULL) {
      if (is.null(private$performance) && !inherits(performance.output, c("MinResult", "ConfMatrix")))
        stop("[", class(self)[1], "][FATAL] Performance output parameter must be ",
             "defined as 'MinResult' or 'ConfMatrix' type. Aborting...")

      if (!is.null(performance.output) && inherits(performance.output, c("MinResult", "ConfMatrix")))
        output <- as.character(performance.output$getTP())
      else output <- as.character(private$performance$getTP())

      names(output) <- class(self)[1]
      output
    }
  )
)
