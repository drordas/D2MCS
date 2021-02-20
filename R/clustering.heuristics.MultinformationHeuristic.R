#' @title Feature-clustering based on Mutual Information Computation theory.
#'
#' @description Performs the feature-clustering using MCC score.
#' Valid for both bi-class and multi-class problems. Only valid for bi-class
#' problems.
#'
#' @seealso \code{\link{Dataset}}, \code{\link[infotheo]{mutinformation}}
#'
#' @keywords cluster manip
#'
#' @import R6
#'
#' @export MultinformationHeuristic

MultinformationHeuristic <- R6::R6Class(
  classname = "MultinformationHeuristic",
  inherit = GenericHeuristic,
  portable = TRUE,
  public = list(
    #'
    #' @description Empty function used to initialize the object arguments in
    #' runtime.
    #'
    initialize = function() { },
    # Heuristic valid for discrete variables
    #'
    #' @description Mutinformation takes two random variables as input and
    #' computes the mutual information in nats according to the entropy
    #' estimator method.
    #'
    #' @param col1 A vector/factor denoting a random variable or a data.frame
    #' denoting a random vector where columns contain variables/features and
    #' rows contain outcomes/samples.
    #' @param col2 An another random variable or random vector (vector/factor or
    #' data.frame).
    #' @param column.names An optional \link{character} vector with the names of
    #' both columns.
    #'
    #' @return Returns the mutual information I(X;Y) in nats.
    #'
    #' @importFrom infotheo mutinformation
    #'
    heuristic = function(col1, col2, column.names = NULL) {
      if (!(private$isBinary(col1) && private$isBinary(col2))) {
        message("[", class(self)[1], "][WARNING] Columns must be binary. ",
                "Returning NA")
        NA
      } else {
        infotheo::mutinformation(col1, col2)
      }
    }
  )
)
