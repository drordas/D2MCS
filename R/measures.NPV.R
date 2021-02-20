#' @title Computes the Negative Predictive Value.
#'
#' @description Negative Predictive Values are the proportions of negative
#' results in statistics and diagnostic tests that are true negative results.
#'
#' @details \deqn{NPV=\frac{TN}{TN+FN}}
#'
#' @seealso \code{\link{MeasureFunction}}, \code{\link{ClassificationOutput}},
#' \code{\link{ConfMatrix}}
#'
#' @keywords classif math
#'
#' @import R6
#'
#' @export NPV

NPV <- R6::R6Class(
  classname = "NPV",
  inherit = MeasureFunction,
  portable = TRUE,
  public = list(
    #'
    #' @description Method for initializing the object arguments during runtime.
    #'
    #' @param performance.output An optional \code{\link{ConfMatrix}} parameter
    #' to define the type of object used as basis to compute the \code{NPV}
    #' measure.
    #'
    initialize = function(performance.output = NULL) {
      super$initialize(performance.output)
    },
    #'
    #' @description The function computes the \strong{NPV} achieved by the M.L.
    #' model.
    #'
    #' @param performance.output An optional \code{\link{ConfMatrix}}  parameter
    #' to define the type of object used as basis to compute the \strong{NPV}
    #' measure.
    #'
    #' @details This function is automatically invoke by the
    #' \link{ClassificationOutput} object.
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
        performance.output$getConfusionMatrix()$byClass["Neg Pred Value"]
      else private$performance$getConfusionMatrix()$byClass["Neg Pred Value"]
    }
  )
)
