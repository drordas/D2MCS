#' @title <<tittle>>
#'
#' @description PredictionOutput
#'
#' @docType class
#'
#' @format NULL
#'
#' @details <<details>
#'
#' @seealso \code{\link{DDMCS}}
#'
#' @keywords NULL
#'
#' @import R6
#'
#' @export PredictionOutput

PredictionOutput <- R6::R6Class(
  classname = "PredictionOutput",
  portable = TRUE,
  public = list(
    #'
    #' @description <<description>>
    #'
    #' @param predictions <<description>>
    #' @param type <<description>>
    #' @param target <<description>>
    #'
    #' @return <<description>>
    #'
    initialize = function(predictions, type, target) {
      private$predictions <- predictions
      private$type <- type
      private$target <- target
    },
    #'
    #' @description <<description>>
    #'
    #' @return <<description>>
    #'
    getPredictions = function() { private$predictions },
    #'
    #' @description <<description>>
    #'
    #' @return <<description>>
    #'
    getType = function() { private$type },
    #'
    #' @description <<description>>
    #'
    #' @return <<description>>
    #'
    getTarget = function() { private$target }
  ),
  private = list(
    predictions = NULL,
    type = NULL,
    target = NULL
  )
)
