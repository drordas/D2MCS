#' @title Feature-clustering based on Fisher's Exact Test.
#'
#' @description Performs feature-clustering based on Fisher's exact test for testing
#' the null of independence of rows and columns in a contingency table with fixed marginals.
#'
#' @docType class
#'
#' @seealso \code{\link{Dataset}}, \link{fisher.test}
#'
#' @keywords cluster manip
#'
#' @import R6
#'
#' @export FisherTestHeuristic

FisherTestHeuristic <- R6::R6Class(
  classname = "FisherTestHeuristic",
  inherit = GenericHeuristic,
  portable = TRUE,
  public = list(
    #' @description Creates the \link{FisherTestHeuristic} object.
    #'
    #' @return a \link{FisherTestHeuristic} object.
    #'
    initialize = function() { },
    # Heuristic valid for discrete variables
    #'
    #' @description Performs the Fisher's exact test for testing the null of independence
    #' between two columns (col1 and col2).
    #'
    #' @param col1 a numeric vector or matrix required to perform the
    #' clustering operation.
    #' @param col2 a numeric vector or matrix to perform the clustering operation.
    #' @param column.names an optional \link{character} vector with the names of
    #' both columns.
    #'
    #' @return a \link{numeric} vector of length 1 or \link{NA} if an error occurs.
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
