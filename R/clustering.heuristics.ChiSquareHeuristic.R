#' @title <<tittle>>
#'
#' @description ChiSquareHeuristic
#'
#' @docType class
#'
#' @format NULL
#'
#' @details <<details>
#'
#' @seealso \code{\link{GenericHeuristic}}
#'
#' @keywords NULL
#'
#' @import R6
#'
#' @export ChiSquareHeuristic

ChiSquareHeuristic <- R6::R6Class(
  classname = "ChiSquareHeuristic",
  inherit = GenericHeuristic,
  portable = TRUE,
  public = list(
    #' @description <<description>>
    initialize = function() { },
    # Heuristic valid for both discrete and continuous variables
    #'
    #' @param col1 <<description>>
    #' @param col2 <<description>>
    #' @param column.names <<description>>
    #'
    #' @description <<description>>
    #'
    #' @return <<return>>
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
