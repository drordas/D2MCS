#' @title Computes the Positive Predictive Value.
#'
#' @description Positive Predictive Values are the proportions of positive
#' results in statistics and diagnostic tests that are true positive results.
#'
#' @details \deqn{PPV = TP / (TP + FP)}
#'
#' @seealso \code{\link{MeasureFunction}}, \code{\link{ClassificationOutput}},
#' \code{\link{ConfMatrix}}
#'
#' @keywords classif math
#'
#' @import R6
#'
#' @export PPV

PPV <- R6::R6Class(
  classname = "PPV",
  inherit = MeasureFunction,
  portable = TRUE,
  public = list(
    #'
    #' @description Method for initializing the object arguments during runtime.
    #'
    #' @param performance.output An optional \code{\link{ConfMatrix}} parameter
    #' to define the type of object used as basis to compute the \strong{PPV}
    #' measure.
    #'
    initialize = function(performance.output = NULL) {
      super$initialize(performance.output)
    },
    #'
    #' @description The function computes the \strong{PPV} achieved by the M.L.
    #' model.
    #'
    #' @param performance.output An optional \code{\link{ConfMatrix}} parameter
    #' to define the type of object used as basis to compute the \strong{PPV}
    #' measure.
    #'
    #' @details This function is automatically invoke by the
    #' \link{ClassificationOutput} object.
    #'
    #' @return A \link{numeric} vector of size 1 or \link{NULL} if an error
    #' occurred.
    #'
    compute = function(performance.output = NULL) {
      if (is.null(private$performance) && !inherits(performance.output, c("MinResult", "ConfMatrix")))
        stop("[", class(self)[1], "][FATAL] Performance output parameter must be ",
             "defined as 'MinResult' or 'ConfMatrix' type. Aborting...")

      if (!is.null(performance.output) && inherits(performance.output, c("MinResult", "ConfMatrix")))
        output <- performance.output$getConfusionMatrix()$byClass["Pos Pred Value"]
      else output <- private$performance$getConfusionMatrix()$byClass["Pos Pred Value"]

      names(output) <- class(self)[1]
      output
    }
  )
)
