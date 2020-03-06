#' @title High Dimensional Subset handler.
#'
#' @description Creates a high dimensional subset from a \link{HDDataset} object.
#' Only the required instances are loaded in memory to avoid
#' unnecesary use of resources and memory.
#'
#' @docType class
#'
#' @format NULL
#'
#' @details Use \link{HDDataset} to ensure the creation of a valid \code{\link{HDSubset}} object.
#'
#' @seealso \code{\link{HDDataset}}, \code{\link{DatasetLoader}}
#'
#' @keywords datasets manip attribute datagen
#'
#' @import R6
#'
#' @export HDSubset
#'
HDSubset <- R6::R6Class(
  classname = "HDSubset",
  portable = TRUE,
  public = list(
    #'
    #' @description Creates the \code{\link{HDSubset}} object.
    #'
    #' @param file.path the name of the file which the data are to be read from.
    #' Each row of the table appears as one line of the file.
    #' If it does not contain an _absolute_ path, the file name is _relative_ to the current
    #' working directory, 'getwd()'.
    #'
    #' @param feature.names a \link{character} vector specifying the name of the features
    #' that should be included in the \link{HDDataset} object.
    #' @param feature.id an integer or character indicating the column (number or name
    #' respectively) identifier. Default \link{NULL} value
    #' is valid ignores defining a identification column.
    #' @param start.at a numeric value to identify the reading start position.
    #' @param sep the field separator character. Values on each line of the file are
    #' separated by this character.
    #' @param chunk.size an integer value indicating the size of chunks taken
    #' over each iteration. By default chunk.size is defied as 10000.
    #'
    #' @return a \link{HDSubset} object.
    #'
    initialize = function(file.path, feature.names, feature.id, start.at = 0,
                          sep = ",", chunk.size) {
      if (is.null(feature.names) || ncol(feature.names) == 0) {
        stop("[", class(self)[1], "][FATAL] Dataset has not being preloaded. ",
             "Aborting...")
      }
      private$chunk.size <- chunk.size
      private$conection <- NULL
      private$file.path <- file.path
      private$feature.names <- names(feature.names)
      private$index <- 0
      private$sep <- sep
      if (isFALSE(feature.id))
        private$feature.id <- NULL
      else private$feature.id <- feature.id

      if (!is.numeric(start.at) || start.at < 0) {
        message("[", class(self)[1], "][WARNING] Starting point must be a ",
                "non-negative numeric value. Assuming 0 as default value")
        private$start.at <- 0
      } else private$start.at <- start.at
    },
    #'
    #' @description get the name of the columns comprising the subset.
    #'
    #' @return a \link{character} vector containing the name of each column.
    #'
    getFeatureNames = function() { private$feature.names },
    #'
    #' @description obtains the number of columns present in the Dataset.
    #'
    #' @return a \link{numeric} value or 0 if is empty.
    #'
    getNcol = function() { length(private$feature.names) },
    #'
    #' @description obtains the column identifier.
    #'
    #' @return a \link{character} vector of size 1.
    #'
    getID = function() { private$feature.names[private$feature.id] },
    #'
    #' @description Creates the \link{FIterator} object.
    #'
    #' @param chunk.size an integer value indicating the size of chunks taken
    #' over each iteration. By default chunk.size is defied as 10000.
    #' @param verbose a logical value to specify if more verbosity is needed.
    #'
    #' @return a \link{FIterator} object to trasverse through \link{HDSubset} instances
    #'
    getIterator = function(chunk.size = private$chunk.size, verbose = FALSE) {
      if (!is.numeric(chunk.size)) {
        message("[", class(self)[1], "][WARNING] Chunk size is not valid. ",
                "Assuming default value")
        chunk.size <- private$chunk.size
      }

      if (!is.logical(verbose)) {
        message("[", class(self)[1], "][WARNING] Verbose type is not valid ",
                "Assuming 'FALSE' as default value")
        verbose <- FALSE
      }

      it.params <- list(file.path = private$file.path,
                        feature.names = private$feature.names,
                        start = private$start.at, sep = private$sep,
                        col.names = self$getFeatureNames())
      FIterator$new(it.params, chunk.size, verbose = verbose)
    },
    #'
    #' @description releases the resources used to manage high dimensional
    #' datasets (such as file handlers).
    #'
    finalize = function() {
      if (!is.null(private$conetion))
        close(private$conection)
      private$conection <- NULL
    },
    #'
    #' @description checks if the subset contains a target class.
    #'
    #' @return a \link{logical} to specify if the subset contains a target class or not.
    #'
    isBlinded = function() { TRUE }
  ),
  private = list(
    feature.names = NULL,
    file.path = NULL,
    conection = NULL,
    index = 0,
    start.at = 0,
    sep = 0,
    data.chunk = NULL,
    chunk.size = 100000,
    feature.id = NULL
  )
)
