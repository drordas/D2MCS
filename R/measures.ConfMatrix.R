#' @title <<tittle>>
#'
#' @description ConfMatrix
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
#' @export ConfMatrix

ConfMatrix <- R6::R6Class(
  classname = "ConfMatrix",
  portable = TRUE,
  public = list(
    #'
    #' @description <<description>>
    #'
    #' @param confMatrix <<description>>
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
    #' @description <<description>>
    #'
    #' @return <<description>>
    #'
    getConfusionMatrix = function() { private$confusionMatrix },
    #'
    #' @description <<description>>
    #'
    #' @return <<description>>
    #'
    getTP = function() { private$confusionMatrix$table[private$positive.class, private$positive.class] },
    #'
    #' @description <<description>>
    #'
    #' @return <<description>>
    #'
    getTN = function() { private$confusionMatrix$table[private$negative.class, private$negative.class] },
    #'
    #' @description <<description>>
    #'
    #' @return <<description>>
    #'
    getFN = function() { private$confusionMatrix$table[private$negative.class, private$positive.class] },
    #'
    #' @description <<description>>
    #'
    #' @return <<description>>
    #'
    getFP = function() { private$confusionMatrix$table[private$positive.class, private$negative.class] }
  ),
  private = list(
    confusionMatrix = NULL,
    positive.class = NULL,
    negative.class = NULL
  )
)
