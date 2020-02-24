#' @title <<tittle>>
#'
#' @description ClusteringStrategy
#'
#' @docType class
#'
#' @format NULL
#'
#' @details <<details>
#'
#' @seealso \code{\link{Subset}}
#'
#' @keywords NULL
#'
#' @import R6
#'
#' @export ClusteringStrategy

ClusteringStrategy <- R6::R6Class(
  classname = "ClusteringStrategy",
  public = list(
    #'
    #' @param subset <<description>>
    #' @param heuristic <<description>>
    #' @param description <<description>>
    #' @param configuration <<description>>
    #'
    #' @description <<description>>
    #'
    initialize = function(subset, heuristic, description, configuration) {

      if (is.null(description) || !is.character(description)) {
        stop("[", class(self)[1], "][FATAL] Strategy description parameter must ",
             "be defined as 'character' type. Aborting...")
      }

      if (!inherits(subset, "Subset")) {
        stop("[", class(self)[1], "][FATAL] Subset parameter must be defined as ",
             "'Subset' type. Aborting...")
      }

      if (is.list(heuristic)) {
        if (length(Filter(function(x) inherits(x, "GenericHeuristic"), heuristic)) == 0) {
          stop("[", class(self)[1], "][FATAL] Adequate heuristics not found ",
               "(must inherit from 'GenericHeuristic' class). Aborting...")
        }
      } else {
        if (inherits(heuristic, "GenericHeuristic")) {
          heuristic <- list(heuristic)
        } else { stop("[", class(self)[1], "][FATAL] Heuristics is not correct ",
                      "(must inherit from 'GenericHeuristic' class). Aborting...") }
      }

      if (!inherits(configuration, "StrategyConfiguration")) {
        stop("[", class(self)[1], "][FATAL] Configuration parameter must be ",
             "inherit from 'StrategyConfiguration' class. Aborting...")
      }

      private$description <- description
      private$subset <- subset
      private$heuristic <- heuristic
      private$configuration <- configuration
    },
    #'
    #' @description <<description>>
    #'
    #' @return <<return>>
    #'
    getDescription = function() { private$description },
    #'
    #' @description <<description>>
    #'
    #' @return <<return>>
    #'
    getHeuristic = function() { private$heuristic },
    #'
    #' @description <<description>>
    #'
    #' @return <<return>>
    #'
    getConfiguration = function() { private$configuration },
    #'
    #' @description <<description>>
    #'
    #' @return <<return>>
    #'
    getBestClusterDistribution = function() { private$best.distribution },
    #'
    #' @description <<description>>
    #'
    #' @return <<return>>
    #'
    getUnclustered = function() { private$not.distribution },
    #'
    #' @param verbose <<description>>
    #' @param ... <<description>>
    #'
    #' @description <<description>>
    #'
    execute = function(verbose, ...) {
      stop("[", class(self)[1], "][FATAL] Class is abstract. ",
           "Method should be defined in inherited class. Aborting...")
    },
    #'
    #' @description <<description>>
    #'
    #' @param num.clusters <<description>>
    #' @param num.groups <<description>>
    #' @param include.unclustered <<description>>
    #'
    #' @return <<return>>
    #'
    getDistribution = function(num.clusters = NULL, num.groups = NULL,
                               include.unclustered = FALSE) {
      stop("[", class(self)[1], "][FATAL] Class is abstract. ",
           "Method should be defined in inherited class. Aborting...")
    },
    #'
    #' @param subset <<description>>
    #' @param num.cluster <<description>>
    #' @param num.groups <<description>>
    #' @param include.unclustered <<description>>
    #'
    #' @description <<description>>
    #'
    createTrain = function(subset, num.cluster = NULL, num.groups = NULL,
                           include.unclustered = FALSE) {
      stop("[", class(self)[1], "][FATAL] Class is abstract. ",
           "Method should be defined in inherited class. Aborting...")
    },
    #'
    #' @description <<description>>
    #'
    #' @param file.name <<description>>
    #' @param ... <<description>>
    #' @param dir.path <<description>>
    #'
    plot = function(dir.path = NULL, file.name = NULL, ...) {
      stop("[", class(self)[1], "][FATAL] Class is abstract. ",
           "Method should be defined in inherited class. Aborting...")
    },
    #'
    #' @description <<description>>
    #'
    #' @param dir.path <<description>>
    #' @param name <<description>>
    #' @param num.clusters <<description>>
    #'
    saveCSV = function(dir.path, name, num.clusters = NULL) {
      stop("[", class(self)[1], "][FATAL] Class is abstract. ",
           "Method should be defined in inherited class. Aborting...")
    }
  ),
  private = list(
    description = NULL,
    subset = NULL,
    heuristic = NULL,
    configuration = NULL,
    all.distribution = NULL,
    best.distribution = NULL,
    not.distribution = NULL
  )
)
