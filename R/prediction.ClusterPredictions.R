#' @title Manages the predictions achieved on a cluster.
#'
#' @description Stores the predictions achieved by the best M.L. of each cluster.
#'
#' @docType class
#'
#' @seealso \code{\link{DDMCS}}, \code{\link{ClassificationOutput}}, \code{\link{Prediction}}
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
    #' @param class.values A \code{\link{character}} vector contaning the values of the target class.
    #' @param positive.class A \code{\link{character}} with the value of the positive class.
    #'
    initialize = function(class.values, positive.class) {

      if (is.null(positive.class) || !(positive.class %in% class.values))
        stop("[", class(self)[1], "][FATAL] Positive class not found. Should be ",
             paste0(class.values, collapse = " or "), ". Aborting...")

      private$positive.class <- positive.class
      private$class.values <- class.values
      private$pred <- list()
    },
    #'
    #' @description The function is used to add the prediction achieved by a specific M.L. model.
    #'
    #' @param prediction A \code{\link{Prediction}} object containing the computed predictions.
    #'
    add = function(prediction) {
      if ("Prediction" %in% class(prediction)) {
        private$pred <- append(private$pred, prediction)
      } else stop("[", class(self)[1], "][FATAL] Prediction parameter must be ",
                 "defined as 'Prediction' object. Aborting... ")
    },
    #'
    #' @description The function returns the predictions placed at specific position.
    #'
    #' @param position A \code{\link{numeric}} value indicating the position of the predicions to be obtained.
    #'
    #' @return A \code{\link{Prediction}} object.
    #'
    get = function(position) {
      if (position > 0 && position <= length(private$pred)) {
        private$pred[[position]]
      } else stop("[", class(self)[1], "][FATAL] Position exceeds list size. Aborting...")
    },
    #'
    #' @description The function returns all the predictions.
    #'
    #' @return A \code{\link{list}} containing all computed predictions.
    #'
    getAll = function() { private$pred },
    #'
    #' @description The function returns the number of computed predictions.
    #'
    #' @return A \code{\link{numeric}} value.
    #'
    size = function() { length(private$pred) },
    #'
    #' @description The function gets the value of the positive class.
    #'
    #' @return A \code{\link{character}} vector of size 1.
    #'
    getPositiveClass = function() { private$positive.class },
    #'
    #' @description The function returns all the values of the target class.
    #'
    #' @return A \code{\link{character}} vector containing all target values.
    #'
    getClassValues = function() { private$class.values }
  ),
  private = list(
    pred = NULL,
    positive.class = NULL,
    class.values = NULL,
    loaded.resources = NULL
  )
)
