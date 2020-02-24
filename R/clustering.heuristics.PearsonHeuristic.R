#' @title <<tittle>>
#'
#' @description PearsonHeuristic
#'
#' @docType class
#'
#' @format NULL
#'
#' @details <<details>
#'
#' @seealso \code{\link{Dataset}}
#'
#' @keywords NULL
#'
#' @import R6
#'
#' @export PearsonHeuristic

PearsonHeuristic <- R6::R6Class(
  classname = "PearsonHeuristic",
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
    #' @importFrom stats cor
    #'
    heuristic = function(col1, col2, column.names = NULL) {
      tryCatch(
      stats::cor(col1, col2, method = "pearson"),
      error = function(e) {
        message("[", class(self)[1], "][ERROR] Error occurred calculating ",
                "pearson heuristic: '", e, "' . Returning NA")
        NA
      })
    }
  )
)
