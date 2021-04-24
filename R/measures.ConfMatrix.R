#' @title Confusion matrix wrapper.
#'
#' @description Creates a \code{\link{R6}} confusion matrix from the
#' \code{\link[caret]{confusionMatrix}} caret package.
#'
#' @seealso \code{\link{D2MCS}}, \code{\link{MeasureFunction}},
#' \code{\link{ClassificationOutput}}
#'
#' @keywords classif math
#'
#' @import R6
#'
#' @export ConfMatrix

ConfMatrix <- R6::R6Class(
  classname = "ConfMatrix",
  portable = TRUE,
  public = list(
    #'
    #' @description Method to create a confusion matrix object from a
    #' \code{caret} \code{\link[caret]{confusionMatrix}}
    #'
    #' @param confMatrix A \code{caret} \link[caret]{confusionMatrix} argument.
    #'
    initialize = function(confMatrix) {
      if (!inherits(confMatrix, "confusionMatrix"))
        stop("[", class(self)[1], "][FATAL] ConfMatrix parameter must be defined ",
             "as 'caret::confusionMatrix' type. Aborting...")

      private$positive.class <- confMatrix$positive
      private$negative.class <- colnames(confMatrix$table)[which(colnames(confMatrix$table) != confMatrix$positive)]
      private$confusionMatrix <- confMatrix
    },
    #'
    #' @description The function obtains the \code{\link[caret]{confusionMatrix}}
    #' following the same structured as defined in the \code{caret} package
    #'
    #' @return A \code{\link[caret]{confusionMatrix}} object.
    #'
    getConfusionMatrix = function() { private$confusionMatrix },
    #'
    #' @description The function is used to compute the number of True Positive
    #' values achieved.
    #'
    #' @return A \link{numeric} vector of size 1.
    #'
    getTP = function() { private$confusionMatrix$table[private$positive.class, private$positive.class] },
    #'
    #' @description The function computes the True Negative values.
    #'
    #' @return A \link{numeric} vector of size 1.
    #'
    getTN = function() { private$confusionMatrix$table[private$negative.class, private$negative.class] },
    #'
    #' @description The function returns the number of Type II errors
    #' (False Negative).
    #'
    #' @return A \link{numeric} vector of size 1.
    #'
    getFN = function() { private$confusionMatrix$table[private$negative.class, private$positive.class] },
    #'
    #' @description The function returns the number of Type I errors
    #' (False Negative).
    #'
    #' @return A \link{numeric} vector of size 1.
    #'
    getFP = function() { private$confusionMatrix$table[private$positive.class, private$negative.class] }
  ),
  private = list(
    confusionMatrix = NULL,
    positive.class = NULL,
    negative.class = NULL
  )
)
