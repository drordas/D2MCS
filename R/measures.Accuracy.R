#' @title Computes the Accuracy measure.
#'
#' @description Computes the ratio of number of correct predictions to the total
#' number of input samples.
#'
#' @details \deqn{Accuracy=\frac{Number Correct Predictions}{Total Number of
#' Predictions}}
#'
#' @seealso \code{\link{MeasureFunction}}, \code{\link{ClassificationOutput}},
#' \code{\link{ConfMatrix}}.
#'
#' @keywords classif math
#'
#' @import R6
#'
#' @export Accuracy

Accuracy <- R6::R6Class(
  classname = "Accuracy",
  inherit = MeasureFunction,
  portable = TRUE,
  public = list(
    #'
    #' @description Method for initializing the object arguments during runtime.
    #'
    #' @param performance.output An optional \code{\link{ConfMatrix}} used as
    #' basis to compute the performance.
    #'
    initialize = function(performance.output = NULL) {
      super$initialize(performance.output)
    },
    #'
    #' @description The function computes the \strong{Accuracy} achieved by the
    #' M.L. model.
    #'
    #' @param performance.output An optional \code{\link{ConfMatrix}} parameter
    #' to define the type of object used as basis to compute the
    #' \strong{Accuracy} measure.
    #'
    #' @details This function is automatically invoke by the
    #' \code{\link{ClassificationOutput}} object.
    #'
    #' @seealso \code{\link{ConfMatrix}}
    #'
    #' @return A \link{numeric} vector of size 1 or \link{NULL} if an error
    #' occurred.
    #'
    compute = function(performance.output = NULL) {
      if (is.null(private$performance) && !inherits(performance.output, c("MinResult", "ConfMatrix")))
        stop("[", class(self)[1], "][FATAL] Performance output parameter must be ",
             "defined as 'MinResult' or 'ConfMatrix' type. Aborting...")

      if (!is.null(performance.output) && inherits(performance.output, c("MinResult", "ConfMatrix")))
        output <- performance.output$getConfusionMatrix()$overall["Accuracy"]
      else output  <- private$performance$getConfusionMatrix()$overall["Accuracy"]

      names(output) <- class(self)[1]
      output
    }
  )
)
