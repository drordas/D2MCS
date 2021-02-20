#' @title Computes the True Negative value.
#'
#' @description This is the number of individuals with a negative condition for
#' which the test result is negative. The value entered here must be non-negative.
#'
#' @seealso \code{\link{MeasureFunction}}, \code{\link{ClassificationOutput}},
#' \code{\link{ConfMatrix}}
#'
#' @keywords classif math
#'
#' @import R6
#'
#' @export TN

TN <- R6::R6Class(
  classname = "TN",
  inherit = MeasureFunction,
  portable = TRUE,
  public = list(
    #'
    #' @description Method for initializing the object arguments during runtime.
    #'
    #' @param performance.output An optional \code{\link{ConfMatrix}} parameter
    #' to define the type of object used to compute the \strong{TN} measure.
    #'
    initialize = function(performance.output = NULL) {
      super$initialize(performance.output)
    },
    #'
    #' @description The function computes the \strong{TN} achieved by the M.L. model.
    #'
    #' @param performance.output An optional \code{\link{ConfMatrix}} parameter
    #' to define the type of object used as basis to compute the \strong{TN}
    #' measure.
    #'
    #' @details This function is automatically invoke by the
    #' \code{\link{ClassificationOutput}} object.
    #'
    #' @seealso \code{\link{ConfMatrix}}
    #'
    #' @return A \link{numeric} vector of size 1 or \link{NULL} if
    #' an error occurred.
    #'
    compute = function(performance.output = NULL) {
      if (is.null(private$performance) && !inherits(performance.output, c("MinResult", "ConfMatrix")))
        stop("[", class(self)[1], "][FATAL] Performance output parameter must be ",
             "defined as 'MinResult' or 'ConfMatrix' type. Aborting...")

      if (!is.null(performance.output) && inherits(performance.output, c("MinResult", "ConfMatrix")))
        output <- as.character(performance.output$getTN())
      else output <- as.character(private$performance$getTN())

      names(output) <- class(self)[1]
      output
    }
  )
)
