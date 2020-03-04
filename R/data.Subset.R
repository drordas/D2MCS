#' @title Classification set.
#'
#' @description The \code{Subset} is used for testing or classification purposes.
#' If a target class is defined the \link{Subset} can be used as test and classification,
#' otherwise the \link{Subset} only classification is compatible.
#'
#' @docType class
#'
#' @details Use \link{Dataset} to ensure the creation of a valid \code{\link{Subset}} object.
#'
#' @seealso \code{\link{Dataset}}, \code{\link{DatasetLoader}}, \code{\link{Trainset}}
#'
#' @keywords datasets manip attribute datagen
#'
#' @import R6
#'
#' @export Subset

Subset <- R6::R6Class(
  classname = "Subset",
  portable = TRUE,
  cloneable = FALSE,
  public = list(
    #'
    #' @description Creates a \link{Subset} object.
    #'
    #' @param dataset a fully filled \link{data.frame}.
    #' @param class.index a \link{numeric} value identifying the column representing
    #' the target class
    #' @param class.values a \link{character} vector containing all the values of the target class.
    #' @param positive.class a \link{character} value representing the positive class value.
    #' @param feature.id a \link{numeric} value specifying the column number used as identifier.
    #'
    #' @return \link{Subset} object.
    #'
    initialize = function(dataset, class.index, class.values,
                          positive.class, feature.id = NULL) {
      if (any(is.null(dataset), nrow(dataset) == 0, !is.data.frame(dataset))) {
        stop("[", class(self)[1], "][FATAL] Dataset empty or incorrect ",
             "(must be a data.frame). Aborting...")
      }

      if (is.null(class.index) ||
          !(class.index %in% c(1:ncol(dataset)))) {
        stop("[", class(self)[1], "][FATAL] Class index paramenter is incorrect. ",
             "Must be between 1 and ", ncol(dataset), ". Aborting...")
      }

      if (!(positive.class %in% as.character(unique(dataset[, class.index])))) {
        stop("[", class(self)[1], "][FATAL] Positive Class parameter is incorrect. ",
             "Must be '", paste(as.character(unique(dataset[, class.index])), collapse = "' '"),
             "'. Aborting...")
      }

      if (!all(class.values %in% as.character(unique(dataset[, class.index])))) {
        stop("[", class(self)[1], "][FATAL] Class values parameter is incorrect. ",
             "Must be '", paste(as.character(unique(dataset[, class.index])), collapse = "' '"),
             "'. Aborting...")
      }

      private$data <- dataset
      private$class.index <- class.index
      private$feature.id <- feature.id
      private$positive.class <- positive.class
      private$class.name <- names(private$data)[private$class.index]
      private$class.values <- class.values
      private$feature.names <- names(private$data[, -private$class.index])
    },
    #'
    #' @description get the name of the columns comprising the subset.
    #'
    #' @return a \link{character} vector containing the name of each column.
    #'
    getFeatureNames = function() { private$feature.names },
    #'
    #' @description get the values of a specific feature.
    #'
    #' @param feature.names a \link{character} vector comprising the name of the
    #' features to be obtained.
    #'
    #' @return a \link{character} vector or NULL if subset is empty.
    #'
    getFeatures = function(feature.names = NULL) {
      if (is.vector(feature.names) && length(feature.names) > 0) {
        private$data[, intersect(names(private$data[, -private$class.index]), feature.names)]
      } else { private$data[, -private$class.index] }
    },
    #'
    #' @description get the column name used as identifier.
    #'
    #' @return a \link{character} vector of size 1 of NULL if column id is not defined.
    #'
    getID = function() {
      if (!is.null(private$feature.id))
        private$feature.names[private$feature.id]
      else private$feature.id
    },
    #'
    #' @description areates the \link{DIterator} object.
    #'
    #' @param chunk.size chunk.size an integer value indicating the size of chunks taken
    #' over each iteration. By default chunk.size is defied as 10000.
    #' @param verbose a logical value to specify if more verbosity is needed.
    #'
    #' @return a \link{DIterator} object to trasverse through \link{Subset} instances.
    #'
    getIterator = function(chunk.size = private$chunk.size, verbose = FALSE) {
      if (!is.numeric(chunk.size)) {
        message("[", class(self)[1], "][WARNING] Chunk size is not valid. ",
                "Assuming default value")
        chunk.size <- private$chunk.size
      }

      if (!is.logical(verbose)) {
        message("[", class(self)[1], "][WARNING] Verbose type is not valid. ",
                "Assuming 'FALSE' as default value")
        verbose <- FALSE
      }
      DIterator$new(data = private$data, chunk.size = chunk.size,
                    verbose = verbose)
    },
    #'
    #' @description gets all the values of the target class.
    #'
    #' @return a \link{factor} vector with all the values of the target class.
    #'
    getClassValues = function() { private$data[, private$class.index] },
    #'
    #' @description the function is used to compute the ratio of each class value
    #' in the \link{Subset}.
    #'
    #' @param target.value the class value used as reference to perform the comparison.
    #'
    #' @return a \link{numeric} value.
    #'
    getClassBalance = function(target.value = NULL) {
      if (is.null(target.value)) {
        target.value <- private$positive.class
      } else {
        if (!(target.value %in% private$class.values)) {
          message("[", class(self)[1], "][WARNING] Target class not found. ",
                  "Assuming default '", private$positive.class, "' value")
          target.value <- private$positive.class
        }
      }
      count <- as.data.frame(t(as.matrix(table(private$data[, private$class.index]))))
      round(count[, target.value] / sum(count[, which(names(count) != target.value)]), digits = 3)
    },
    #'
    #' @description the function is used to obtain the index of the column
    #' containing the target class.
    #'
    #' @return a \link{numeric} value.
    #'
    getClassIndex = function() { private$class.index },
    #'
    #' @description the function is used to specify the name of the column
    #' containing the target class.
    #'
    #' @return a \link{character} value.
    #'
    getClassName = function() { private$class.name },
    #'
    #' @description the function is in charge of obtaining the number of columns
    #' comprising the \link{Subset}. See \link{ncol} for more information.
    #'
    #' @return an \link{integer} of length 1 or \link{NULL}.
    #'
    getNcol = function() { ncol(private$data) },
    #'
    #' @description the function is used to determine the number of rows present in
    #' the \link{Subset}. See \link{nrow} for more information.
    #'
    #' @return an \link{integer} of length 1 or \link{NULL}.
    #'
    getNrow = function() { nrow(private$data) },
    #'
    #' @description the function returns the value of the positive class.
    #'
    #' @return a \link{character} vector of size 1 or \link{NULL} if not defined.
    #'
    getPositiveClass = function() { private$positive.class },
    #'
    #' @description the function is used to check if the \link{Subset} contains
    #' a target class.
    #'
    #' @return a \link{logical} value where \link{TRUE} represents the abscense
    #' of target class and \link{FALSE} its presence.
    #'
    isBlinded = function() { FALSE }
  ),
  private = list(
    data = NULL,
    class.index = NULL,
    class.name = NULL,
    feature.names = NULL,
    class.values = NULL,
    positive.class = NULL,
    chunk.size = 10000,
    feature.id = NULL
  )
)
