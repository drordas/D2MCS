#' @title Abstract class to computing performance across resamples.
#'
#' @description Abstract used as template to define customized metrics to
#' compute model performance during train.
#'
#' @details This class is an archetype, so it cannot be instantiated.
#'
#' @seealso \code{\link{NoProbability}}, \code{\link{UseProbability}}
#'
#' @keywords misc
#'
#' @import R6
#'
#' @export SummaryFunction

SummaryFunction <- R6::R6Class(
  classname = "SummaryFunction",
  portable = TRUE,
  public = list(
    # '
    #' @description The function carries out the initialization of parameters
    #' during runtime.
    #'
    #' @param measures A \link{character} vector with the measures used.
    #'
    initialize = function(measures) {
      if (is.null(measures))
        stop("[", class(self)[1], "][FATAL] Measures were not defined. Aborting...")
      private$measures <- measures
    },
    #'
    #' @description Abstract function used to implement the performance
    #' calculator method. To guarantee a proper operation, this method is
    #' automatically invoked by \code{\link{D2MCS}} framework.
    #'
    execute = function() {
      stop("[", class(self)[1], "][FATAL] Class is abstract. ",
           "Method should be defined in inherited class. Aborting...")
    },
    #'
    #' @description The function obtains the measures used to compute the
    #' performance across resamples.
    #'
    #' @return A \link{character} vector of \link{NULL} if measures are not
    #' defined.
    #'
    getMeasures = function() {
      private$measures
    }
  ),
  private = list(
    measures = NULL
  )
)
