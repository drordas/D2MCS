#' @title Computes the Specificity Value.
#'
#' @description Specificity is defined as the proportion of actual negatives, which got predicted
#' as the negative (or true negative). This implies that there will be another proportion of
#' actual negative, which got predicted as positive and could be termed as false positives.
#'
#' @details \deqn{Specificity = \frac{(True Negative)}{(True Negative + False Positive)}}
#'
#' @seealso \code{\link{MeasureFunction}}, \code{\link{ClassificationOutput}}, \code{\link{ConfMatrix}}
#'
#' @keywords classif math
#'
#' @import R6
#'
#' @export Specificity

Specificity <- R6::R6Class(
  classname = "Specificity",
  inherit = MeasureFunction,
  portable = TRUE,
  public = list(
    #'
    #' @description Method for initializing the object arguments during runtime.
    #'
    #' @param performance.output An optional \code{\link{ConfMatrix}} parameter to define the type of
    #' object used as basis to compute the measure.
    #'
    #' @return An \code{\link{Specificity}} object.
    #'
    initialize = function(performance.output = NULL) {
      super$initialize(performance.output)
    },
    #'
    #' @description The function computes the \code{Specificity} achieved by the M.L. model.
    #'
    #' @param performance.output An optional \code{\link{ConfMatrix}} parameter to define the
    #' type of object used as basis to compute the \code{Specificity} measure.
    #' @details This function is automatically invoque by the \link{ClassificationOutput} object.
    #' @seealso \code{\link{ConfMatrix}}
    #' @return A \code{\link{numeric}} vector of size 1 or \code{\link{NULL}} if an error occured.
    #'
    compute = function(performance.output = NULL) {
      if (is.null(private$performance) && !inherits(performance.output, c("MinResult", "ConfMatrix")))
        stop("[", class(self)[1], "][FATAL] Performance output parameter must be ",
             "defined as 'MinResult' or 'ConfMatrix' type. Aborting...")

      if (!is.null(performance.output) && inherits(performance.output, c("MinResult", "ConfMatrix")))
        output <- performance.output$getConfusionMatrix()$byClass["Specificity"]
      else output <- private$performance$getConfusionMatrix()$byClass["Specificity"]

      names(output) <- class(self)[1]
      output
    }
  )
)
