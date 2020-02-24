#' @title <<tittle>>
#'
#' @description GainRatioHeuristic
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
#' @export GainRatioHeuristic

GainRatioHeuristic <- R6::R6Class(
  classname = "GainRatioHeuristic",
  inherit = GenericHeuristic,
  portable = TRUE,
  public = list(
    #' @description <<description>>
    initialize = function() { },
    # Heuristic valid for continuous variables (col2) where col1 is binary or categorical
    #'
    #' @param col1 <<description>>
    #' @param col2 <<description>>
    #' @param column.names <<description>>
    #'
    #' @description <<description>>
    #'
    #' @return <<return>>
    #' @importFrom FSelector gain.ratio
    #'
    heuristic = function(col1, col2, column.names = NULL) {
      data <- as.data.frame(cbind(col1, col2))
      names(data) <- column.names
      tryCatch(
      FSelector::gain.ratio(as.formula(sprintf("`%s` ~.", column.names[2])), data)$attr_importance,
      error = function(e) {
        message("[", class(self)[1], "][ERROR] Error occurred calculating ",
                "gain.ratio heuristic: '", e, "' . Returning NA")
        NA
      })
    }
  )
)
