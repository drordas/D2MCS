#' @title Feature-clustering based on Pearson Correlation Test.
#'
#' @description Performs the feature-clustering using Pearson correlation tests.
#' Valid for both, bi-class and multi-class problems.
#'
#' @docType class
#'
#' @format NULL
#'
#' @details The test statistic is based on Pearson's product moment correlation
#' coefficient cor(x, y) and follows a t distribution with length(x)-2 degrees of
#' freedom if the samples follow independent normal distributions.
#' If there are at least 4 complete pairs of observation,
#' an asymptotic confidence interval is given based on Fisher's Z transform.
#'
#' @seealso \code{\link{Dataset}} \link{cor.test}
#'
#' @keywords cluster manip
#'
#' @import R6
#'
#' @export PearsonHeuristic

PearsonHeuristic <- R6::R6Class(
  classname = "PearsonHeuristic",
  inherit = GenericHeuristic,
  portable = TRUE,
  public = list(
    #'
    #' @description Creates a \link{PearsonHeuristic} object.
    #' @return a \link{PearsonHeuristic} object.
    #'
    initialize = function() { },
    # Heuristic valid for both discrete and continuous variables
    #'
    #' @description Test for association between paired samples using Pearsons test.
    #' @param col1 a \link{numeric} vector or matrix required to perform the
    #' clustering operation.
    #' @param col2 a \link{numeric} vector or matrix to perform the clustering operation.
    #' @param column.names an optional \link{character} vector with the names of
    #' both columns.
    #' @return a \link{numeric} vector of length 1 or \link{NA} if an error occurs.
    #'
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
