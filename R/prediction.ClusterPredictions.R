#' @title Manages the predictions achieved on a cluster.
#'
#' @description Stores the predictions achieved by the best M.L. of each cluster.
#'
#' @seealso \code{\link{DDMCS}}, \code{\link{ClassificationOutput}},
#' \code{\link{Prediction}}
#'
#' @keywords methods math
#'
#' @import R6
#'
#' @export ClusterPredictions

ClusterPredictions <- R6::R6Class(
  classname = "ClusterPredictions",
  portable = TRUE,
  public = list(
    #'
    #' @description Method for initializing the object arguments during runtime.
    #'
    #' @param class.values A \link{character} vector containing the values of
    #' the target class.
    #' @param positive.class A \link{character} with the value of the positive
    #' class.
    #'
    initialize = function(class.values, positive.class) {

      if (is.null(positive.class) || !(positive.class %in% class.values)) {
        stop("[", class(self)[1], "][FATAL] Positive class not found. Should be ",
             paste0(class.values, collapse = " or "), ". Aborting...")
      }

      private$positive.class <- positive.class
      private$class.values <- class.values
      private$pred <- list()
    },
    #'
    #' @description The function is used to add the prediction achieved by a
    #' specific M.L. model.
    #'
    #' @param prediction A \code{\link{Prediction}} object containing the
    #' computed predictions.
    #'
    add = function(prediction) {
      if (!"Prediction" %in% class(prediction)) {
        stop("[", class(self)[1], "][FATAL] Prediction parameter must be ",
             "defined as 'Prediction' object. Aborting... ")
      }
      private$pred <- append(private$pred, prediction)
    },
    #'
    #' @description The function returns the predictions placed at specific
    #' position.
    #'
    #' @param position A \link{numeric} value indicating the position of the
    #' predictions to be obtained.
    #'
    #' @return A \code{\link{Prediction}} object.
    #'
    get = function(position) {
      if (!all(position > 0, position <= length(private$pred))) {
        stop("[", class(self)[1], "][FATAL] Position exceeds list size. Aborting...")
      }
      private$pred[[position]]
    },
    #'
    #' @description The function returns all the predictions.
    #'
    #' @return A \link{list} containing all computed predictions.
    #'
    getAll = function() { private$pred },
    #'
    #' @description The function returns the number of computed predictions.
    #'
    #' @return A \link{numeric} value.
    #'
    size = function() { length(private$pred) },
    #'
    #' @description The function gets the value of the positive class.
    #'
    #' @return A \link{character} vector of size 1.
    #'
    getPositiveClass = function() { private$positive.class },
    #'
    #' @description The function returns all the values of the target class.
    #'
    #' @return A \link{character} vector containing all target values.
    #'
    getClassValues = function() { private$class.values }
  ),
  private = list(
    pred = NULL,
    positive.class = NULL,
    class.values = NULL
  )
)
