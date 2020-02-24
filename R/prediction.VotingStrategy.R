#' @title <<tittle>>
#'
#' @description VotingStrategy
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
#' @export VotingStrategy

VotingStrategy <- R6::R6Class(
  classname = "VotingStrategy",
  portable = TRUE,
  public = list(
    #'
    #' @description <<description>>
    #'
    initialize = function() {
      # private$final.pred <- FinalPred$new()
    },
    #'
    #' @description <<description>>
    #'
    #' @return <<description>>
    #'
    getVotingSchemes = function() { private$voting.schemes },
    #'
    #' @description <<description>>
    #'
    #' @return <<description>>
    #'
    getMetrics = function() { private$metrics },
    #'
    #' @description <<description>>
    #'
    #' @param predictions <<description>>
    #' @param ... <<description>>
    #'
    #' @return <<description>>
    #'
    execute = function(predictions, ...) {
      stop("[", class(self)[1], "][FATAL] Class is abstract. ",
           "Method should be defined in inherited class. Aborting...")
    },
    #'
    #' @description <<description>>
    #'
    #' @return <<description>>
    #'
    getName = function() { class(self)[1] }
  ),
  private = list(
    voting.schemes = NULL,
    metrics = NULL
  )
)
