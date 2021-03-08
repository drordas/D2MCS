#' @title Computes the Sensitivity Value.
#'
#' @description Sensitivity is a measure of the proportion of actual positive
#' cases that got predicted as positive (or true positive).
#'
#' @details \deqn{Sensitivity = TP / (TP + FN)}
#'
#' @seealso \code{\link{MeasureFunction}}, \code{\link{ClassificationOutput}},
#' \code{\link{ConfMatrix}}
#'
#' @keywords classif math
#'
#' @import R6
#'
#' @export Sensitivity

Sensitivity <- R6::R6Class(
  classname = "Sensitivity",
  inherit = MeasureFunction,
  portable = TRUE,
  public = list(
    #'
    #' @description Method for initializing the object arguments during runtime.
    #'
    #' @param performance.output An optional \code{\link{ConfMatrix}} parameter
    #' to define the type of object used as basis to compute the
    #' \code{Sensitivity} measure.
    #'
    initialize = function(performance.output = NULL) {
      super$initialize(performance.output)
    },
    #'
    #' @description The function computes the \strong{Sensitivity} achieved by the
    #' M.L. model.
    #'
    #' @param performance.output An optional \code{\link{ConfMatrix}} parameter
    #' to define the type of object used as basis to compute the
    #' \strong{Sensitivity} measure.
    #'
    #' @details This function is automatically invoke by the
    #' \link{ClassificationOutput} object.
    #'
    #' @return A \link{numeric} vector of size 1 or \link{NULL} if
    #' an error occurred.
    #'
    compute = function(performance.output = NULL) {
      if (is.null(private$performance) && !inherits(performance.output, c("MinResult", "ConfMatrix")))
        stop("[", class(self)[1], "][FATAL] Performance output parameter must be ",
             "defined as 'MinResult' or 'ConfMatrix' type. Aborting...")

      if (!is.null(performance.output) && inherits(performance.output, c("MinResult", "ConfMatrix")))
        output <- performance.output$getConfusionMatrix()$byClass["Sensitivity"]
      else output <- private$performance$getConfusionMatrix()$byClass["Sensitivity"]

      names(output) <- class(self)[1]
      output
    }
  )
)
