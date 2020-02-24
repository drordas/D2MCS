#' @title <<tittle>>
#'
#' @description ClusterPredictions
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
#' @export ClusterPredictions

ClusterPredictions <- R6::R6Class(
  classname = "ClusterPredictions",
  portable = TRUE,
  public = list(
    #'
    #' @description <<description>>
    #'
    #' @param class.values <<description>>
    #' @param positive.class <<description>>
    #'
    initialize = function(class.values, positive.class) {

      if (!(positive.class %in% class.values))
        stop("[", class(self)[1], "][FATAL] Positive class not found. Should be ",
             paste0(unique(class.values), collapse = " or "), ". Aborting...")

      private$positive.class <- positive.class
      private$class.values <- class.values
      private$pred <- list()
    },
    #'
    #' @description <<description>>
    #'
    #' @param prediction <<description>>
    #'
    #' @return <<description>>
    #'
    add = function(prediction) {
      if ("Prediction" %in% class(prediction)) {
        private$pred <- append(private$pred, prediction)
      } else stop("[", class(self)[1], "][FATAL] Prediction parameter must be ",
                 "defined as 'Prediction' object. Aborting... ")
    },
    #'
    #' @description <<description>>
    #'
    #' @param position <<description>>
    #'
    #' @return <<description>>
    #'
    get = function(position) {
      if (position > 0 && position <= length(private$pred)) {
        private$pred[[position]]
      } else stop("[", class(self)[1], "][FATAL] Position exceeds list size. Aborting...")
    },
    #'
    #' @description <<description>>
    #'
    #' @return <<description>>
    #'
    getAll = function() { private$pred },
    #'
    #' @description <<description>>
    #'
    #' @return <<description>>
    #'
    size = function() { length(private$pred) },
    #'
    #' @description <<description>>
    #'
    #' @return <<description>>
    #'
    getPositiveClass = function() { private$positive.class },
    #'
    #' @description <<description>>
    #'
    #' @return <<description>>
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
