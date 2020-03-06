#' @title Feature-clustering based on InformationGain methodology.
#'
#' @description Performs the feature-clustering using entropy-based filters.
#'
#' @docType class
#'
#' @format NULL
#'
#' @seealso \code{\link{Dataset}}, \code{\link[FSelector]{information.gain}}
#'
#' @keywords cluster manip
#'
#' @import R6
#'
#' @export InformationGainHeuristic

InformationGainHeuristic <- R6::R6Class(
  classname = "InformationGainHeuristic",
  inherit = GenericHeuristic,
  portable = TRUE,
  public = list(
    #' @description Creates a \link{InformationGainHeuristic} object.
    #' @return a \link{InformationGainHeuristic} object.
    initialize = function() { },
    #'
    #' @description The algorithm find weights of discrete attributes basing on
    #' their correlation with continous class attribute. Particularly
    #' Information Gain uses \code{H(Class) + H(Attribute) - H(Class, Attribute)}
    #'
    #' @param col1 a \link{numeric} vector or matrix required to perform the
    #' clustering operation.
    #' @param col2 a \link{numeric} vector or matrix to perform the clustering operation.
    #' @param column.names an optional \link{character} vector with the names of
    #' both columns.
    #'
    #' @return a \link{numeric} vector of length 1 or \link{NA} if an error occurs.
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
