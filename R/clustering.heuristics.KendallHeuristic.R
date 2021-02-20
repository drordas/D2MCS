#' @title Feature-clustering based on Kendall Correlation Test.
#'
#' @description Performs the feature-clustering using Kendall correlation tests.
#'
#' @details The method estimate the association between paired samples and
#' compute a test of the value being zero. They use different measures of
#' association, all in the range [-1, 1] with 0 indicating no association.
#' Method valid only for bi-class problems.
#'
#' @seealso \code{\link{Dataset}}, \code{\link[stats]{cor.test}}
#'
#' @keywords cluster manip
#'
#' @import R6
#'
#' @export KendallHeuristic

KendallHeuristic <- R6::R6Class(
  classname = "KendallHeuristic",
  inherit = GenericHeuristic,
  portable = TRUE,
  public = list(
    #'
    #' @description Empty function used to initialize the object arguments in
    #' runtime.
    #'
    initialize = function() { },
    # Heuristic valid for continuous variables
    #'
    #' @description Test for association between paired samples using Kendall's
    #' tau value.
    #'
    #' @param col1 A \link{numeric} vector or matrix required to perform the
    #' clustering operation.
    #' @param col2 A \link{numeric} vector or matrix to perform the clustering
    #' operation.
    #' @param column.names An optional \link{character} vector with the names of
    #' both columns.
    #'
    #' @return a \link{numeric} vector of length 1 or \link{NA} if an error
    #' occurs.
    #'
    #' @importFrom stats cor.test
    #'
    heuristic = function(col1, col2, column.names = NULL) {
      if (private$isBinary(col1) || !private$isBinary(col2)) {
        message("[", class(self)[1], "][WARNING] Columns must be real. ",
                "Returning NA")
        NA
      } else {
        tryCatch(
        unname(stats::cor.test(col1, col2, method = "kendall")$estimate, force = TRUE),
        error = function(e) {
          message("[", class(self)[1], "][ERROR] Error occurred calculating ",
                  "kendall heuristic: '", e, "' . Returning NA")
          NA
        })
      }
    }
  )
)
