#' @title Computes the Kappa Cohen value.
#'
#' @description Cohen's Kappa measures the agreement between two raters who each classify
#' N items into C mutually exclusive categories.
#'
#' @details \deqn{\kappa \equiv \frac{p_o-p_e}{1-p_e} = 1 - \frac{1-p_0}{1-p_e}}
#'
#' @seealso \code{\link{MeasureFunction}}, \code{\link{ClassificationOutput}}, \code{\link{ConfMatrix}}
#'
#' @keywords classif math
#'
#' @import R6
#'
#' @export Kappa

Kappa <- R6::R6Class(
  classname = "Kappa",
  inherit = MeasureFunction,
  portable = TRUE,
  public = list(
    #'
    #' @description Method for initializing the object arguments during runtime.
    #'
    #' @param performance.output An optional \code{\link{ConfMatrix}} used as basis
    #' to compute the performance.
    #'
    #' @return An \code{\link{Kappa}} object.
    #'
    initialize = function(performance.output = NULL) {
      super$initialize(performance.output)
    },
    #'
    #' @description The function computes the \code{Kappa} achieved by the M.L. model.
    #'
    #' @param performance.output An optional \code{\link{ConfMatrix}} parameter to define the type
    #' of object used as basis to compute the \code{Kappa} measure.
    #' @details This function is automatically invoked by the \link{ClassificationOutput} object.
    #' @seealso \code{\link{ConfMatrix}}
    #' @return A \code{\link{numeric}} vector of size 1 or \code{\link{NULL}} if an error occured.
    #'
    compute = function(performance.output = NULL) {
      if (is.null(private$performance) && !inherits(performance.output, c("MinResult", "ConfMatrix")))
        stop("[", class(self)[1], "][FATAL] Performance output parameter must be ",
             "defined as 'MinResult' or 'ConfMatrix' type. Aborting...")

      if (!is.null(performance.output) && inherits(performance.output, c("MinResult", "ConfMatrix")))
        output <- performance.output$getConfusionMatrix()$overall["Kappa"]
      else output <- private$performance$getConfusionMatrix()$overall["Kappa"]

      names(output) <- class(self)[1]
      output
    }
  )
)
