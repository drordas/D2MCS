#' @title Abstract Feature Clustering Strategy class.
#'
#' @description Abstract class used as a template to ensure the proper definition
#' of new customized clustering strategies.
#'
#' @docType class
#'
#' @details The \link{ClusteringStrategy} is an archetype class so it cannot be instantiated.
#'
#' @seealso \code{\link{Subset}}, \code{\link{GenericHeuristic}}
#'
#' @keywords cluster manip
#'
#' @import R6
#'
#' @export ClusteringStrategy

ClusteringStrategy <- R6::R6Class(
  classname = "ClusteringStrategy",
  public = list(
    #'
    #' @description A function responsible for creating a \link{ClusteringStrategy} object.
    #'
    #' @param subset a \link{Subset} object to perform the clustering strategy.
    #' @param heuristic the heuristic to be applied. Must inherite from \link{GenericHeuristic} class.
    #' @param description a \link{character} vector describing the strategy operation.
    #' @param configuration optional customized configuration parameters for the strategy.
    #' Must inherited from \link{StrategyConfiguration} abstract class.
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
    #' @description the function is used to obtain the description of the strategie.
    #'
    #' @return a \link{character} vector of \link{NULL} if not defined.
    #'
    getDescription = function() { private$description },
    #'
    #' @description the function returns the heuristic applied for the clustering strategy.
    #'
    #' @return an object inherited from \link{ClusteringStrategy} class.
    #'
    getHeuristic = function() { private$heuristic },
    #'
    #' @description the function returns the configuration parameters used to perform the clustering strategy.
    #'
    #' @return an object inherited from \link{StrategyConfiguration} class.
    #'
    getConfiguration = function() { private$configuration },
    #'
    #' @description the function obtains the best clustering distribution.
    #'
    #' @return a \link{list} of clusters. Each list element represents a feature group.
    #'
    getBestClusterDistribution = function() { private$best.distribution },
    #'
    #' @description the function is used to return the features that cannot be clustered due to
    #' incompatibilities with the used heuristic.
    #'
    #' @return a \link{character} vector containing the unclassified features.
    #'
    getUnclustered = function() { private$not.distribution },
    #'
    #' @param verbose a \link{logical} value to specify if more verbosity is needed.
    #' @param ... further arguments passed down to \code{execute} function.
    #'
    #' @description abstract function responsible of performing the clustering
    #' strategy over the defined \link{Subset}.
    #'
    execute = function(verbose, ...) {
      stop("[", class(self)[1], "][FATAL] Class is abstract. ",
           "Method should be defined in inherited class. Aborting...")
    },
    #'
    #' @description abstract function used to obtain the set of features following
    #' an specific clustering distribution.
    #'
    #' @param num.clusters a \link{numeric} value to select the number of clusters (define the distribution).
    #' @param num.groups a single or \link{numeric} vector value to identify a specific group that
    #' forms the clustering distribution.
    #' @param include.unclustered a \link{logical} value to determine if unclustered features should be included.
    #'
    #' @return a \link{list} with the features comprising an specific clustering distribution.
    #'
    getDistribution = function(num.clusters = NULL, num.groups = NULL,
                               include.unclustered = FALSE) {
      stop("[", class(self)[1], "][FATAL] Class is abstract. ",
           "Method should be defined in inherited class. Aborting...")
    },
    #'
    #' @param subset A \link{Subset} object used as a basis to create the \link{Trainset}
    #' @param num.cluster A \link{numeric} value to select the number of clusters (define the distribution).
    #' @param num.groups A single or \link{numeric} vector value to identify a specific group that
    #' forms the clustering distribution.
    #' @param include.unclustered A \link{logical} value to determine if unclustered features should be included.
    #'
    #' @description Abstract function in charge of creating a \link{Trainset} object for training purposes.
    #'
    createTrain = function(subset, num.cluster = NULL, num.groups = NULL,
                           include.unclustered = FALSE) {
      stop("[", class(self)[1], "][FATAL] Class is abstract. ",
           "Method should be defined in inherited class. Aborting...")
    },
    #'
    #' @description Abstract function responsible of creating a plot to visualize the clustering distribution.
    #'
    #' @param dir.path An optional \link{character} argument to define the name of the directory where the exported plot will be saved.
    #' If not defined, the file path will be automatically assigned to the current working directory, 'getwd()'.
    #' @param file.name The name of the PDF file where the plot is exported.
    #' @param ... Further arguments passed down to \code{execute} function.
    #'
    plot = function(dir.path = NULL, file.name = NULL, ...) {
      stop("[", class(self)[1], "][FATAL] Class is abstract. ",
           "Method should be defined in inherited class. Aborting...")
    },
    #'
    #' @description abstract function to save the clustering distribution to a CSV file.
    #'
    #' @param dir.path The name of the directory to save the CSV file.
    #' @param name Defines the name of the CSV file.
    #' @param num.clusters An optional parameter to select the number of clusters to be saved.
    #' If not defined, all clusters will be saved.
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
