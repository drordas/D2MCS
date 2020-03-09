#' @title Encapsulates the achieved predictions.
#'
#' @description The class used to encapsulates all the computed predictions to facilitate
#' their access and maintenance.
#'
#' @seealso \code{\link{DDMCS}}
#'
#' @keywords math misc
#'
#' @import R6
#'
#' @export PredictionOutput

PredictionOutput <- R6::R6Class(
  classname = "PredictionOutput",
  portable = TRUE,
  public = list(
    #'
    #' @description Method for initializing the object arguments during runtime.
    #'
    #' @param predictions A \code{\link{list}} of \code{\link{FinalPred}} elements.
    #' @param type A \code{\link{character}} to define which type of predictions should be returned. If not defined
    #' all type of probabilities will be returned. Conversely if "prob" or "raw" is defined then computed 'probabilistic' or
    #' 'class' values are returned.
    #' @param target A \code{\link{character}} defining the value of the positive class.
    #'
    #' @return An \code{\link{PredictionOutput}} object.
    #'
    initialize = function(predictions, type, target) {
      private$predictions <- predictions
      private$type <- type
      private$target <- target
    },
    #'
    #' @description The function returns the final predictions.
    #'
    #' @return A \code{\link{list}} containing the final predictions or
    #' \code{\link{NULL}} if classification stage was not succesfully performed.
    #'
    getPredictions = function() { private$predictions },
    #'
    #' @description The function returns the type of prediction should be returned.
    #' If "prob" or "raw" is defined then computed 'probabilistic' or
    #' 'class' values are returned.
    #'
    #' @return A \code{\link{character}} value.
    #'
    getType = function() { private$type },
    #'
    #' @description The function returns the value of the target class.
    #'
    #' @return  A \code{\link{character}} value.
    #'
    getTarget = function() { private$target }
  ),
  private = list(
    predictions = NULL,
    type = NULL,
    target = NULL
  )
)
