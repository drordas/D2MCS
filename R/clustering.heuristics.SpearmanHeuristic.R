#' @title <<tittle>>
#'
#' @description SpearmanHeuristic
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
#' @export SpearmanHeuristic

SpearmanHeuristic <- R6::R6Class(
  classname = "SpearmanHeuristic",
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
    #' @importFrom stats cor.test
    #'
    heuristic = function(col1, col2, column.names = NULL) {
      if (!is.numeric(col1) || !is.numeric(col2)) {
        message("[", class(self)[1], "][WARNING] Columns must be 'numeric' type. ",
                "Returning NA")
        NA
      } else {
        tryCatch(
        stats::cor.test(col1, col2, method = "spearman", exact = FALSE)$p.value,
        error = function(e) {
          message("[", class(self)[1], "][ERROR] Error occurred calculating ",
                  "spearman heuristic: '", e, "' . Returning NA")
          NA
        })
      }
    }
  )
)
