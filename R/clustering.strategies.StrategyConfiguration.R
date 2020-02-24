#' @title <<tittle>>
#'
#' @description StrategyConfiguration
#'
#' @docType class
#'
#' @format NULL
#'
#' @details <<details>
#'
#' @seealso \code{\link{DependencyBasedStrategyConfiguration}}
#'
#' @keywords NULL
#'
#' @import R6
#'
#' @export StrategyConfiguration

StrategyConfiguration <- R6::R6Class(
  classname = "StrategyConfiguration",
  portable = TRUE,
  public = list(
    #' @description
    initialize = function() { },
    #'
    #' @description <<description>>
    #'
    #' @param ... <<description>>
    #'
    #' @return <<return>>
    #'
    minNumClusters = function(...) {
      message("[", class(self)[1], "][INFO] Using default minCluster configuration: 2 clusters minimun")
      2
    },
    #'
    #' @description <<description>>
    #'
    #' @param ... <<description>>
    #'
    #' @return <<return>>
    #'
    maxNumClusters = function(...) {
      message("[", class(self)[1], "][INFO] Using default maxCluster configuration: 50 clusters maximun")
      50
    }
  )
)
