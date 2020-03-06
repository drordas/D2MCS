#' @title Custom Strategy Configuration handler for the DependencyBasedStrategy strategy.
#'
#' @description Define the default configuration parameters for the \link{DependencyBasedStrategy} strategy.
#'
#' @docType class
#'
#' @format NULL
#'
#' @seealso \code{\link{StrategyConfiguration}}, \code{\link{DependencyBasedStrategy}}
#'
#' @keywords cluster manip
#'
#' @import R6
#'
#' @export DependencyBasedStrategyConfiguration

DependencyBasedStrategyConfiguration <- R6::R6Class(
  classname = "DependencyBasedStrategyConfiguration",
  inherit = StrategyConfiguration,
  portable = TRUE,
  public = list(
    #'
    #' @description Empty function used to initalize the object arguments during runtime.
    #'
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
      features <- eval.parent(substitute(alist(...))[["features"]])
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
      features <- eval.parent(substitute(alist(...))[["features"]])
      max <- max(lengths(features))
      if (is.infinite(max)) { 3 } else { max }
    },
    #'
    #' @description The function is used to define the interval to consider the dependency between binary features.
    #'
    #' @param ... Further arguments passed down to \code{maxNumClusters} function.
    #'
    #' @return A \link{numeric} vector of length 1.
    #'
    getBinaryCutoff = function() { 0.6 },
    #'
    #' @description The function allows defining the cutoff to consider the dependency between real features.
    #'
    #' @param ... Further arguments passed down to \code{maxNumClusters} function.
    #'
    #' @return A \link{numeric} vector of length 1.
    #'
    getRealCutoff = function() { 0.7 },
    #'
    #' @description The function solves the ties between two (or more) features.
    #'
    #' @param feature A \link{character} containing the name of the feature
    #' @param clus.candidates A single or \link{numeric} vector value to identify
    #' the candidate groups to insert the feature.
    #' @param fea.dep.dist.clus A \link{list} containing the groups chosen for the features.
    #' @param corpus A \link{data.frame} containing the features of the initial data.
    #' @param heuristic The heuristic used to compute the relevance of each feature.
    #' Must inherit from \link{GenericHeuristic} abstract class.
    #' @param class a \link{character} vector containing all the values of the target class.
    #' @param class.name A \link{character} value representing the name of the target class.
    #'
    tiebreak = function(feature, clus.candidates, fea.dep.dist.clus, corpus,
                        heuristic, class, class.name) {
      private$lfdcTiebreak(feature, clus.candidates, fea.dep.dist.clus,
                           corpus, heuristic)
    },
    #'
    #' @description The function determines the quality of a cluster.
    #'
    #' @param clusters A \link{list} with the feature distribution of each cluster.
    #' @param metrics A numeric \link{list} with the metrics associated to the cluster (dependency between all features and dependency between the features and the class).
    #'
    #' @return A \link{numeric} vector of length 1.
    #'
    qualityOfCluster = function(clusters, metrics) {
      mean(metrics[["dep.tar"]])
    },
    #'
    #' @description The function indicates if clustering is getting better as the number of them increases.
    #'
    #' @param clusters.deltha A \link{numeric} vector value with the quality values of the built clusters.
    #'
    #' @return A \link{numeric} vector of length 1.
    #'
    isImprovingClustering = function(clusters.deltha) {
      clusters.deltha <- clusters.deltha * 100

      diff <- clusters.deltha[[length(clusters.deltha)]] - min(clusters.deltha)
      # If it does not worsen more than 0.01 %
      ifelse(0.01 > diff, TRUE, FALSE)

    }
  ),
  private = list(
    getFeaturesInCluster = function(features, cluster) {
      features.return <- c()
      for (fea in names(features)) {
        if (cluster %in% features[[fea]]) {
          features.return <- c(features.return, fea)
        }
      }
      features.return
    },
    lfdcTiebreak = function(feature, clus.candidates, fea.dep.dist.clus, corpus, heuristic) {
      # Search for the cluster set with less dependence on the features
      means.cluster <- list()
      for (clus in clus.candidates) {
        mean <- 0
        pos <- 0
        for (feature.cluster in private$getFeaturesInCluster(fea.dep.dist.clus, clus)) {
          result.heuristic <- abs(heuristic$heuristic(corpus[, feature],
                                                      corpus[, feature.cluster],
                                                      column.names = c(feature,
                                                                       feature.cluster)))
          mean <- (mean * pos + result.heuristic) / (pos + 1)
          pos <- pos + 1
        }
        means.cluster <- append(means.cluster, mean)
      }
      append(fea.dep.dist.clus[[feature]], clus.candidates[[which.min(means.cluster)]])
    },
    ltdcTiebreak = function(feature, clus.candidates, fea.dep.dist.clus, corpus,
                            heuristic, class, class.name) {
      # Search for the cluster set with less dependence with the target
      means.cluster <- list()
      for (clus in clus.candidates) {
        mean <- 0
        pos <- 0
        for (feature.cluster in append(private$getFeaturesInCluster(fea.dep.dist.clus, clus), feature)) {
          result.heuristic <- abs(heuristic$heuristic(corpus[, feature.cluster],
                                                      class,
                                                      column.names = c(feature.cluster,
                                                                       class.name)))
          mean <- (mean * pos + result.heuristic) / (pos + 1)
          pos <- pos + 1
        }
        means.cluster <- append(means.cluster, mean)
      }
      append(fea.dep.dist.clus[[feature]], clus.candidates[[which.min(means.cluster)]])
    }
  )
)
