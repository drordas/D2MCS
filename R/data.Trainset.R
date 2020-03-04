#' @title Trainning set.
#'
#' @description The \code{Trainset} is used to perform training operations over M.L. models.
#' A target class should be defined to guarantee a full compatibility with supervised models.
#'
#' @docType class
#'
#' @format NULL
#'
#' @details Use \link{Dataset} object to ensure the creation of a valid \code{\link{Trainset}} object.
#'
#' @seealso \link{Dataset}, \link{DatasetLoader}, \link{Subset}, \link{ClusteringStrategy}
#'
#' @keywords datasets manip attribute programming utilities
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
    #' @description Creates a \link{Trainset} object.
    #'
    #' @param cluster.dist the type of cluster distribution used as basis
    #' to build the \link{Trainset}. See \link{ClusteringStrategy} for more information.
    #' @param class.name used to specify the name of the column containing the target class.
    #' @param class.values specifies all the posible values of the target class.
    #' @param positive.class defines the positive class value.
    #'
    #' @return a fully operative \link{Trainset} object.
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
    #' @description the function is used to obtain the value of the positive class.
    #'
    #' @return a \link{numeric} value with the positive class value.
    #'
    getPositiveClass = function() { private$positive.class },
    #'
    #' @description the function is used to return the name of the target class.
    #'
    #' @return a \link{character} vector with length 1.
    #'
    getClassName = function() { private$class.name },
    #'
    #' @description the function is used to compute all the posible target class values.
    #'
    #' @return a \link{factor} value.
    #'
    getClassValues = function() { private$class.values },
    #'
    #' @description the function returns the name of the columns comprising
    #' an specific cluster distribution.
    #'
    #' @param num.cluster a numeric value used to specify the cluster number
    #' of the cluster distribution used when creating the \link{Trainset}.
    #'
    #' @return a \link{character} vector with all column names.
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
    #' @description the function returns the values of the columns comprising
    #' an specific cluster distribution. Target class is omitted.
    #'
    #' @param num.cluster a numeric value used to specify the cluster number
    #' of the cluster distribution used when creating the \link{Trainset}.
    #'
    #' @return a \link{data.frame} with the values of the features comprising
    #' the selected cluster distribution.
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
    #' @description the function returns the values of the columns comprising
    #' an specific cluster distribution. Target class is included as the last column.
    #'
    #' @param num.cluster num.cluster a numeric value used to specify the cluster number
    #' of the cluster distribution used when creating the \link{Trainset}.
    #'
    #' @return a \link{data.frame} with the values of the features comprising the
    #' selected cluster distribution.
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
    #' @description the function obtains the number of groups (clusters) that forms
    #' the cluster distribution.
    #'
    #' @return a \link{numeric} vector of size 1.
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
