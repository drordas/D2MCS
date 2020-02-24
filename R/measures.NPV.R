#' @title <<tittle>>
#'
#' @description NPV
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
#' @export NPV

NPV <- R6::R6Class(
  classname = "NPV",
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
        performance.output$getConfusionMatrix()$byClass["Neg Pred Value"]
      else private$performance$getConfusionMatrix()$byClass["Neg Pred Value"]
    }
  )
)
