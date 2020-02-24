#' @title <<tittle>>
#'
#' @description DependencyBasedStrategyConfiguration
#'
#' @docType class
#'
#' @format NULL
#'
#' @details <<details>
#'
#' @seealso \code{\link{StrategyConfiguration}}
#'
#' @keywords NULL
#'
#' @import R6
#'
#' @export DependencyBasedStrategyConfiguration

DependencyBasedStrategyConfiguration <- R6::R6Class(
  classname = "DependencyBasedStrategyConfiguration",
  inherit = StrategyConfiguration,
  portable = TRUE,
  public = list(
    #' @description <<description>>
    initialize = function() { },
    #'
    #' @description <<description>>
    #'
    #' @param ... <<description>>
    #'
    #' @return <<return>>
    #'
    minNumClusters = function(...) {
      features <- eval.parent(substitute(alist(...))[["features"]])
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
      features <- eval.parent(substitute(alist(...))[["features"]])
      max <- max(lengths(features))
      if (is.infinite(max)) { 3 } else { max }
    },
    #'
    #' @description <<description>>
    #'
    #' @param ... <<description>>
    #'
    #' @return <<return>>
    #'
    getBinaryCutoff = function() { 0.6 },
    #'
    #' @description <<description>>
    #'
    #' @param ... <<description>>
    #'
    #' @return <<return>>
    #'
    getRealCutoff = function() { 0.7 },
    #'
    #' @param feature <<description>>
    #' @param clus.candidates <<description>>
    #' @param fea.dep.dist.clus <<description>>
    #' @param corpus <<description>>
    #' @param heuristic <<description>>
    #' @param class <<description>>
    #' @param class.name <<description>>
    #'
    #' @description <<description>>
    #'
    #' @return <<return>>
    #'
    tiebreak = function(feature, clus.candidates, fea.dep.dist.clus, corpus,
                        heuristic, class, class.name) {
      private$lfdcTiebreak(feature, clus.candidates, fea.dep.dist.clus,
                           corpus, heuristic)
      # private$ltdcTiebreak(feature, clus.candidates, fea.dep.dist.clus, corpus, heuristic, class, class.name)
    },
    #'
    #' @param clusters <<description>>
    #' @param metrics <<description>>
    #'
    #' @description <<description>>
    #'
    #' @return <<return>>
    #'
    qualityOfCluster = function(clusters, metrics) {
      mean(metrics[["dep.tar"]])
    },
    #'
    #' @param clusters.deltha <<description>>
    #'
    #' @description <<description>>
    #'
    #' @return <<return>>
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
