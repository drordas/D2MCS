#' @title <<tittle>>
#'
#' @description FP
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
#' @export FP

FP <- R6::R6Class(
  classname = "FP",
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

      if (!is.null(performance.output) && inherits(performance.output, c("MinResult", "ConfMatrix")))
        output <- as.character(performance.output$getFP())
      else output <- as.character(private$performance$getFP())

      names(output) <- class(self)[1]
      output
    }
  )
)
