#' @title Feature-clustering based on Spearman Correlation Test.
#'
#' @description Performs the feature-clustering using Spearman's rho statistic.
#'
#' @docType class
#'
#' @details  Spearman's rho statistic is to estimate a rank-based measure of
#' association. These tests may be used if the data do not necessarily come from a
#' bivariate normal distribution.
#'
#' @seealso \code{\link{Dataset}} \link{cor.test}
#'
#' @keywords cluster manip
#'
#' @import R6
#'
#' @export SpearmanHeuristic

SpearmanHeuristic <- R6::R6Class(
  classname = "SpearmanHeuristic",
  inherit = GenericHeuristic,
  portable = TRUE,
  public = list(
    #'
    #' @description Creates a \link{SpearmanHeuristic} object.
    #' @return a \link{SpearmanHeuristic} object.
    #'
    initialize = function() { },
    # Heuristic valid for both discrete and continuous variables
    #'
    #' @description Test for correlation between paired samples using Spearman rho statistic.
    #'
    #' @param col1 \link{numeric} vectors of data values.
    #' @param col2 \link{numeric} vectors of data values with same length as col1.
    #' @param column.names an optional \link{character} vector with the names of
    #' both columns.
    #'
    #' @return a \link{numeric} vector of length 1 or \link{NA} if an error occurs.
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
