#' @title Feature-clustering based on ChiSquare method.
#'
#' @description Performs feature-clustering based on ChiSquare method.
#'
#' @seealso \code{\link{GenericHeuristic}} \link{chisq.test}
#'
#' @keywords cluster manip
#'
#' @import R6
#'
#' @export ChiSquareHeuristic

ChiSquareHeuristic <- R6::R6Class(
  classname = "ChiSquareHeuristic",
  inherit = GenericHeuristic,
  portable = TRUE,
  public = list(
    #' @description Creates a \link{ChiSquareHeuristic} class.
    initialize = function() { },
    # Heuristic valid for both discrete and continuous variables
    #'
    #' @description functions responsible of performing the ChiSquare feature-clustering
    #' operation.
    #'
    #' @param col1 a \link{numeric} vector or matrix required to perform the
    #' clustering operation.
    #' @param col2 a \link{numeric} vector or matrix to perform the clustering operation.
    #' @param column.names an optional \link{character} vector with the names of
    #' both columns.
    #'
    #' @return a \link{numeric} vector of length 1 or \link{NA} if an error occurs.
    #' @importFrom stats chisq.test
    #'
    heuristic = function(col1, col2, column.names = NULL) {
      tryCatch(
        # WARNING! Chi-squared approximation may be incorrect
        stats::chisq.test(col1, col2)$p.value,
        error = function(e) {
          message("[", class(self)[1], "][ERROR] Error occurred calculating ",
                  "chi.square heuristic: '", e, "' . Returning NA")
          NA
        })
    }
  )
)
