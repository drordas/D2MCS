#' @title <<tittle>>
#'
#' @description MCCHeuristic
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
#' @export MCCHeuristic

MCCHeuristic <- R6::R6Class(
  classname = "MCCHeuristic",
  inherit = GenericHeuristic,
  portable = TRUE,
  public = list(
    #' @description <<description>>
    initialize = function() { },
    # Heuristic valid for discrete variables
    #'
    #' @param col1 <<description>>
    #' @param col2 <<description>>
    #' @param column.names <<description>>
    #'
    #' @description <<description>>
    #'
    #' @return <<return>>
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
