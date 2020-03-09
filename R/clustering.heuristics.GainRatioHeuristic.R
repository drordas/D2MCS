#' @title Feature-clustering based on GainRatio methodology.
#'
#' @description Performs the feature-clustering using entropy-based filters.
#'
#' @seealso \code{\link{Dataset}}, \code{\link[FSelector]{gain.ratio}}
#'
#' @keywords cluster manip
#'
#' @import R6
#'
#' @export GainRatioHeuristic

GainRatioHeuristic <- R6::R6Class(
  classname = "GainRatioHeuristic",
  inherit = GenericHeuristic,
  portable = TRUE,
  public = list(
    #' @description Creates the \link{GainRatioHeuristic} object.
    #' @return a \link{GainRatioHeuristic} object.
    #'
    initialize = function() { },
    # Heuristic valid for continuous variables (col2) where col1 is binary or categorical
    #'
    #' @description The algorithms find weights of discrete attributes basing on their
    #' correlation with continous class attribute.
    #'
    #' @param col1 a \link{numeric} vector or matrix required to perform the
    #' clustering operation.
    #' @param col2 a \link{numeric} vector or matrix to perform the clustering operation.
    #' @param column.names an optional \link{character} vector with the names of
    #' both columns.
    #'
    #' @return a \link{numeric} vector of length 1 or \link{NA} if an error occurs.
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
