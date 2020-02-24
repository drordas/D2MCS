#' @title <<tittle>>
#'
#' @description MCC
#'
#' @docType class
#'
#' @format NULL
#'
#' @details <<details>
#'
#' @seealso \code{\link{MeasureFunction}}
#'
#' @keywords NULL
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
    #' @description <<description>>
    #'
    #' @param performance.output <<description>>
    #'
    initialize = function(performance.output = NULL) {
      super$initialize(performance.output)
    },
    #'
    #' @description <<description>>
    #'
    #' @param performance.output <<description>>
    #'
    #' @return <<description>>
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
