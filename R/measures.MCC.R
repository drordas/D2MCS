#' @title Computes the Matthews correlation coefficient.
#'
#' @description The Matthews correlation coefficient is used in machine learning as a
#' measure of the quality of binary (two-class) classifications. It takes into account
#' true and false positives and negatives and is generally regarded as a balanced measure
#' which can be used even if the classes are of very different sizes. The MCC is in essence
#' a correlation coefficient between the observed and predicted binary classifications;
#' it returns a value between −1 and +1.
#'
#' @docType class
#'
#' @details \deqn{MCC = \frac{TP\timesTN-FP\timesFN}{\sqrt{(TP+FP)\times(TP+FN)\times(TN+FP)\times(TN+FN)}}}
#'
#' @seealso \code{\link{MeasureFunction}}, \code{\link{ClassificationOutput}}, \code{\link{ConfMatrix}}
#'
#' @keywords classif math
#'
#' @import R6
#'
#' @export MCC

MCC <- R6::R6Class(
  classname = "MCC",
  inherit = MeasureFunction,
  portable = TRUE,
  public = list(
    #'
    #' @description Method for initializing the object arguments during runtime.
    #'
    #' @param performance.output An optional \code{\link{ConfMatrix}} parameter
    #' used as basis to compute the \code{MCC} measure.
    #'
    #' @return An \code{\link{MCC}} object.
    #'
    initialize = function(performance.output = NULL) {
      super$initialize(performance.output)
    },
    #'
    #' @description The function computes the \code{MCC} achieved by the M.L. model.
    #'
    #' @param performance.output An optional \code{\link{ConfMatrix}} parameter to define
    #' the type of object used as basis to compute the \code{MCC} measure.
    #' @details This function is automatically invoque by the \link{ClassificationOutput} object.
    #' @seealso \code{\link{ConfMatrix}}
    #' @return A \code{\link{numeric}} vector of size 1 or \code{\link{NULL}} if an error occured.
    #'
    compute = function(performance.output = NULL) {
      if (is.null(private$performance) && !inherits(performance.output, c("MinResult", "ConfMatrix")))
        stop("[", class(self)[1], "][FATAL] Performance output parameter must be ",
             "defined as 'MinResult' or 'ConfMatrix' type. Aborting...")

      if (inherits(performance.output, c("MinResult", "ConfMatrix")))
        output <- mltools::mcc(TP = performance.output$getTP(), FP = performance.output$getFP(),
                                FN = performance.output$getFN(), TN = performance.output$getTN())
      else output <- mltools::mcc(TP = private$performance$getTP(), FP = private$performance$getFP(),
                                   FN = private$performance$getFN(), TN = private$performance$getTN())
      names(output) <- class(self)[1]
      output
    }
  )
)
