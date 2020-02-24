#' @title <<tittle>>
#'
#' @description InformationGainHeuristic
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
#' @export InformationGainHeuristic

InformationGainHeuristic <- R6::R6Class(
  classname = "InformationGainHeuristic",
  inherit = GenericHeuristic,
  portable = TRUE,
  public = list(
    #' @description <<description>>
    initialize = function() { },
    #'
    #' @param col1 <<description>>
    #' @param col2 <<description>>
    #' @param column.names <<description>>
    #'
    #' @description <<description>>
    #'
    #' @return <<return>>
    #'
    #' @importFrom FSelector information.gain
    #'
    heuristic = function(col1, col2, column.names = NULL) {
      data <- as.data.frame(cbind(col1, col2))
      names(data) <- column.names
      tryCatch(
      FSelector::information.gain(as.formula(sprintf("`%s` ~.", column.names[2])), data)$attr_importance,
      error = function(e) {
        message("[", class(self)[1], "][ERROR] Error occurred calculating ",
                "information.gain heuristic: '", e, "' . Returning NA")
        NA
      })
    }
  )
)
