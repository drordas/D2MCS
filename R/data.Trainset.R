#' @title <<tittle>>
#'
#' @description Trainset
#'
#' @docType class
#'
#' @format NULL
#'
#' @details <<details>
#'
#' @seealso \code{\link{Dataset}}
#'
#' @keywords NULL
#'
#' @import R6
#'
#' @export Trainset

Trainset <- R6::R6Class(
  classname = "Trainset",
  portable = TRUE,
  cloneable = FALSE,
  public = list(
    #'
    #' @description <<description>>
    #'
    #' @param cluster.dist <<description>>
    #' @param class.name <<description>>
    #' @param class.values <<description>>
    #' @param positive.class <<description>>
    #'
    initialize = function(cluster.dist, class.name, class.values, positive.class) {

      if (!is.vector(cluster.dist) || length(cluster.dist) == 0) {
        stop("[", class(self)[1], "][FATAL] Clusters empty or incorrect (must be a list). ",
             "Aborting...")
      }

      if (!(positive.class %in% as.character(unique(class.values)))) {
        stop("[", class(self)[1], "][FATAL] Positive Class parameter is incorrect. Must be '",
             paste0(as.character(unique(class.values)), collapse = "' '"), "'. Aborting...")
      }

      private$clusters <- cluster.dist
      private$positive.class <- as.character(positive.class)
      private$class.name <- class.name
      private$class.values <- class.values
    },
    #'
    #' @description <<description>>
    #'
    #' @return <<return>>
    #'
    getPositiveClass = function() { private$positive.class },
    #'
    #' @description <<description>>
    #'
    #' @return <<return>>
    #'
    getClassName = function() { private$class.name },
    #'
    #' @description <<description>>
    #'
    #' @return <<return>>
    #'
    getClassValues = function() { private$class.values },
    #'
    #' @description <<description>>
    #'
    #' @param num.cluster <<description>>
    #'
    #' @return <<return>>
    #'
    getFeatureNames = function(num.cluster) {
      if (any(!is.numeric(num.cluster),
                !num.cluster %in% c(1:length(private$clusters)))) {
        stop("[", class(self)[1], "][FATAL] Position not defined or incorrect. ",
             "Must be included between 1 and ", length(private$clusters),
             ". Aborting...")
      }
      names(private$clusters[[num.cluster]])
    },
    #'
    #' @description <<description>>
    #'
    #' @param num.cluster <<description>>
    #'
    #' @return <<return>>
    #'
    getFeatureValues = function(num.cluster) {
      if (any(!is.numeric(num.cluster),
               !num.cluster %in% c(1:length(private$clusters))))
      {
        stop("[", class(self)[1], "][FATAL] Position not defined or incorrect. ",
             "Must be included between 1 and ", length(private$clusters),
             ". Aborting...")
      }
      private$clusters[[num.cluster]]
    },
    #'
    #' @description <<description>>
    #'
    #' @param num.cluster <<description>>
    #'
    #' @return <<return>>
    #'
    getInstances = function(num.cluster) {
      if (any(is.null(num.cluster), !is.numeric(num.cluster),
               !num.cluster %in% c(1:length(private$clusters))))
      {
        stop("[", class(self)[1], "][FATAL] Position not defined or incorrect. ",
             "Must be included between 1 and ", length(private$clusters),
             ". Aborting...")
      }
      instances <- cbind(private$clusters[[num.cluster]], private$class.values)
      names(instances)[length(instances)] <- private$class.name
      instances
    },
    #'
    #' @description <<description>>
    #'
    #' @return <<return>>
    #'
    getNumClusters = function() { length(private$clusters) }
  ),
  private = list(
    clusters = list(),
    class.name = NULL,
    class.values = NULL,
    positive.class = NULL
  )
)
