#' @title Computes the Recall Value.
#'
#' @description Recall (also known as sensitivity) is the fraction of the total
#' amount of relevant instances that were actually retrieved.
#'
#' @details \deqn{recall = TP / (TP + FN)}
#'
#' @seealso \code{\link{MeasureFunction}}, \code{\link{ClassificationOutput}},
#' \code{\link{ConfMatrix}}
#'
#' @keywords classif math
#'
#' @import R6
#'
#' @export Recall

Recall <- R6::R6Class(
  classname = "Recall",
  inherit = MeasureFunction,
  portable = TRUE,
  public = list(
    #'
    #' @description Method for initializing the object arguments during runtime.
    #'
    #' @param performance.output An optional \code{\link{ConfMatrix}} parameter
    #' to define the type of object used as basis to compute the measure.
    #'
    initialize = function(performance.output = NULL) {
      super$initialize(performance.output)
    },
    #'
    #' @description The function computes the \strong{Recall} achieved by the M.L.
    #' model.
    #'
    #' @param performance.output An optional \code{\link{ConfMatrix}} parameter
    #' to define the type of object used as basis to compute the \strong{Recall}
    #' measure.
    #'
    #' @details This function is automatically invoke by the
    #' \code{\link{ClassificationOutput}} object.
    #'
    #' @return A \link{numeric} vector of size 1 or \link{NULL} if
    #' an error occurred.
    #'
    compute = function(performance.output = NULL) {
      if (is.null(private$performance) && !inherits(performance.output, c("MinResult", "ConfMatrix")))
        stop("[", class(self)[1], "][FATAL] Performance output parameter must be ",
             "defined as 'MinResult' or 'ConfMatrix' type. Aborting...")

      if (!is.null(performance.output) && inherits(performance.output, c("MinResult", "ConfMatrix")))
        output <- performance.output$getConfusionMatrix()$byClass["Recall"]
      else output <- private$performance$getConfusionMatrix()$byClass["Recall"]

      names(output) <- class(self)[1]
      output
    }
  )
)
