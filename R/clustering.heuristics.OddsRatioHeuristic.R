#' @title Feature-clustering based on Odds Ratio measure.
#'
#' @description Performs the feature-clustering using Odds Ratio methodology.
#' Valid only for bi-class problems.
#'
#' @docType class
#'
#' @seealso \code{\link{Dataset}}, \code{\link[questionr]{odds.ratio}}
#'
#' @keywords cluster manip
#'
#' @import R6
#'
#' @export OddsRatioHeuristic

OddsRatioHeuristic <- R6::R6Class(
  classname = "OddsRatioHeuristic",
  inherit = GenericHeuristic,
  portable = TRUE,
  public = list(
    #'
    #' @description Creates a \link{OddsRatioHeuristic} object.
    #' @return a \link{OddsRatioHeuristic} object.
    #'
    initialize = function() { },
    #'
    #' @description Calculates the Odds Ratio method.
    #'
    #' @param col1 object from whom odds ratio will be computed
    #' @param col2 a second \link{factor} or \link{numeric} object.
    #' @param column.names column.names an optional \link{character} vector with the names of
    #' both columns.
    #'
    #' @return a \link{numeric} vector of length 1 or \link{NA} if an error occurs.
    #' @importFrom questionr odds.ratio
    #'
    heuristic = function(col1, col2, column.names = NULL) {
      if (!private$isBinary(col1) || !private$isBinary(col2)) {
        message("[", class(self)[1], "][WARNING] Columns must be binary. ",
                "Returning NA")
        NA
      } else {
        # data <- as.data.frame(cbind(col1, col2))
        # names(data) <- column.names
        tryCatch(
        questionr::odds.ratio(table(col1, col2))$p,
        error = function(e) {
          message("[", class(self)[1], "][ERROR] Error occurred calculating ",
                  "odds.ratio heuristic: '", e, "' . Returning NA")
          NA
        })
      }
    }
  )
)
