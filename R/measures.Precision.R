#' @title Computes the Precision Value.
#'
#' @description Precision is the fraction of relevant instances among the retrieved instances
#'
#' @details \deqn{precision = \frac{TP}{TP+FP}}
#'
#' @seealso \code{\link{MeasureFunction}}, \code{\link{ClassificationOutput}},
#' \code{\link{ConfMatrix}}
#'
#' @keywords classif math
#'
#' @import R6
#'
#' @export Precision

Precision <- R6::R6Class(
  classname = "Precision",
  inherit = MeasureFunction,
  portable = TRUE,
  public = list(
    #'
    #' @description Method for initializing the object arguments during runtime.
    #'
    #' @param performance.output An optional \code{\link{ConfMatrix}} parameter
    #' to define the type of object used as basis to compute the measure.
    #'
    #' @return An \code{\link{Precision}} object.
    #'
    initialize = function(performance.output = NULL) {
      super$initialize(performance.output)
    },
    #'
    #' @description The function computes the \code{Precision} achieved by the M.L. model.
    #'
    #' @param performance.output An optional \code{\link{ConfMatrix}} parameter to define
    #' the type of object used as basis to compute the \code{Precision} measure.
    #' @details This function is automatically invoque by the \link{ClassificationOutput} object.
    #' @seealso \code{\link{ConfMatrix}}
    #' @return A \code{\link{numeric}} vector of size 1 or \code{\link{NULL}} if an error occured.
    #'
    compute = function(performance.output = NULL) {
      if (is.null(private$performance) && !inherits(performance.output, c("MinResult", "ConfMatrix")))
        stop("[", class(self)[1], "][FATAL] Performance output parameter must be ",
             "defined as 'MinResult' or 'ConfMatrix' type. Aborting...")

      if (!is.null(performance.output) && inherits(performance.output, c("MinResult", "ConfMatrix")))
        output <- performance.output$getConfusionMatrix()$byClass["Precision"]
      else output <- private$performance$getConfusionMatrix()$byClass["Precision"]

      names(output) <- class(self)[1]
      output
    }
  )
)
