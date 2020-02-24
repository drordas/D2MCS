#' @title <<tittle>>
#'
#' @description FisherTestHeuristic
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
#' @export FisherTestHeuristic

FisherTestHeuristic <- R6::R6Class(
  classname = "FisherTestHeuristic",
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
    #'
    #'
    #' @description <<description>>
    #'
    #' @return <<return>>
    #' @importFrom stats fisher.test
    #'
    heuristic = function(col1, col2, column.names = NULL) {
      if (private$isBinary(col1) && private$isBinary(col2)) {
        col2.factor <- factor(col2)
        col1.factor <- factor(col1, levels =  levels(col2.factor))
        tryCatch(
        stats::fisher.test(col1.factor, col2.factor)$p.value,
        # stats::fisher.test(table(col1.factor, col2.factor))$p.value
        error = function(e) {
          message("[", class(self)[1], "][ERROR] Error occurred calculating ",
                  "fisher.test heuristic: '", e, "' . Returning NA")
          NA
        })
      } else {
        if (!private$isBinary(col1))
          message("[", class(self)[1], "][WARNING] Column '",
                  column.names[1], "' is not binary. Returning NA")
        else message("[", class(self)[1], "][WARNING] Column '",
                     column.names[2], "' is not binary. Returning NA")
        NA
      }
    }
  )
)
