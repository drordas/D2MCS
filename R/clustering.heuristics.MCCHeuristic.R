#' @title Feature-clustering based on Matthews Correlation Coefficient score.
#'
#' @description Performs the feature-clustering using MCC score.
#' Valid for both bi-class and multi-class problems
#'
#' @docType class
#'
#' @format NULL
#'
#' @seealso \code{\link{Dataset}}, \code{\link[mccr]{mccr}}
#'
#' @keywords cluster manip
#'
#' @import R6
#'
#' @export MCCHeuristic

MCCHeuristic <- R6::R6Class(
  classname = "MCCHeuristic",
  inherit = GenericHeuristic,
  portable = TRUE,
  public = list(
    #' @description Creates a \link{MCCHeuristic} object.
    #' @return a \link{MCCHeuristic} object.
    initialize = function() { },
    # Heuristic valid for discrete variables
    #'
    #' @description Calculates the Matthews correlation Coefficient (MCC) score.
    #' @param col1 a numeric vector or matrix required to perform the
    #' clustering operation.
    #' @param col2 a numeric vector or matrix to perform the clustering operation.
    #' @param column.names an optional \link{character} vector with the names of
    #' both columns.
    #'
    #' @return a \link{numeric} vector of length 1 or \link{NA} if an error occurs.
    #' @importFrom mccr mccr
    #'
    heuristic = function(col1, col2, column.names = NULL) {
      if (!(private$isBinary(col1) && private$isBinary(col2))) {
        message("[", class(self)[1], "][WARNING] Columns must be binary. ",
                "Returning NA")
        NA
      } else {
        mccr::mccr(col1, col2)
      }
    }
  )
)
