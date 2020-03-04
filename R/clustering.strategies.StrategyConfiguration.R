#' @title Default Strategy Configuration handler.
#'
#' @description Define default configuration parameters for the clustering strategies.
#'
#' @docType class
#'
#' @format NULL
#'
#' @details The \link{StrategyConfiguration} can be used to define the default configuration parameters for
#' a feature clustering strategy or as an archetype to define new customized parameters.
#'
#' @seealso \code{\link{DependencyBasedStrategyConfiguration}}
#'
#' @import R6
#'
#' @keywords cluster manip
#'
#' @export StrategyConfiguration

StrategyConfiguration <- R6::R6Class(
  classname = "StrategyConfiguration",
  portable = TRUE,
  public = list(
    #'
    #' @description Empty function used to initalize the object arguments during runtime.
    #' @return A \link{StrategyConfiguration} object.
    #'
    initialize = function() { },
    #'
    #' @description Function used to return the minimum number of clusters distributions used. By default the minimum is set in 2.
    #'
    #' @param ... Further arguments passed down to \code{minNumClusters} function.
    #'
    #' @return A \link{numeric} vector of length 1.
    #'
    minNumClusters = function(...) {
      message("[", class(self)[1], "][INFO] Using default minCluster configuration: 2 clusters minimun")
      2
    },
    #'
    #' @description The function is responsible of returning the maximum number of cluster distributions used.
    #' By default the maximum number is set in 50.
    #'
    #' @param ... Further arguments passed down to \code{maxNumClusters} function.
    #'
    #' @return A \link{numeric} vector of length 1.
    #'
    maxNumClusters = function(...) {
      message("[", class(self)[1], "][INFO] Using default maxCluster configuration: 50 clusters maximun")
      50
    }
  )
)
