#' @title Voting Strategy template.
#'
#' @description Abstract class used to define new \code{\link{SingleVoting}} and  \code{\link{CombinedVoting}} schemes.
#'
#' @docType class
#'
#' @seealso \code{\link{DDMCS}}, \code{\link{SingleVoting}}, \code{\link{CombinedVoting}}
#'
#' @keywords models methods math
#'
#' @import R6
#'
#' @export VotingStrategy

VotingStrategy <- R6::R6Class(
  classname = "VotingStrategy",
  portable = TRUE,
  public = list(
    #'
    #' @description Abstract method used to initialize the object arguments during runtime.
    #'
    initialize = function() { },
    #'
    #' @description The function returns the voting schemes that will participate in the voting strategy.
    #'
    #' @return A vector of object inheriting from \code{\link{VotingStrategy}} class.
    #'
    getVotingSchemes = function() { private$voting.schemes },
    #'
    #' @description The function is used to get the metric that will be used during the voting strategy.
    #'
    #' @return A \code{\link{character}} vector.
    #'
    getMetrics = function() { private$metrics },
    #'
    #' @description Abstract function used to implement the operation of the voting schemes.
    #'
    #' @param predictions A \code{\link{ClusterPredictions}} object containing de prediction achieved for each cluster.
    #' @param ... Further arguments passed down to \code{execute} function.
    #'
    execute = function(predictions, ...) {
      stop("[", class(self)[1], "][FATAL] Class is abstract. ",
           "Method should be defined in inherited class. Aborting...")
    },
    #'
    #' @description The function returns the name of the voting scheme.
    #'
    #' @return A \code{\link{character}} vector of size 1.
    #'
    getName = function() { class(self)[1] }
  ),
  private = list(
    voting.schemes = NULL,
    metrics = NULL
  )
)
