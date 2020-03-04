#' @title High Dimensional Dataset handler
#'
#' @description Creates a high dimensional dataset object.
#' Only the required instances are loaded in memory to avoid
#' unnecesary of resources and memmory
#'
#' @docType class
#'
#' @format NULL
#'
#' @details <<details>
#'
#' @seealso \code{\link{Dataset}}, \code{\link{HDSubset}}, \code{\link{DatasetLoader}}
#'
#' @keywords datasets manip attribute datagen
#'
#' @import R6
#'
#' @export HDDataset
#'
HDDataset <- R6::R6Class(
  classname = "HDDataset",
  portable = TRUE,
  cloneable = FALSE,
  public = list(
    #'
    #' @description Creates the \code{\link{HDDataset}} object.
    #'
    #' @param filepath filepath the name of the file which the data are to be read from.
    #' Each row of the table appears as one line of the file.
    #' If it does not contain an _absolute_ path, the file name is _relative_ to the current
    #' working directory, ‘getwd()’.
    #' @param header a logical value indicating whether the file contains the names of the variables as its first line.
    #' If missing, the value is determined from the file format: ‘header’ is set to ‘TRUE’ if and only if the
    #' first row contains one fewer field than the number of columns.
    #' @param sep the field separator character. Values on each line of the file are separated by this character.
    #' @param skip defines the number of header lines should be skipped.
    #' @param normalize.names a logical value indicating whether the columns names should be automatically renamed
    #' to ensure R compatibility.
    #' @param ignore.columns specify the columns from the input file that should be ignored.
    #'
    #' @return a \link{HDDataset} object.
    #'
    initialize = function(filepath, header = TRUE, sep = ",", skip = 0,
                          normalize.names = FALSE, ignore.columns = NULL)
    {
      if (!file.exists(filepath)) {
        stop("[", class(self)[1], "][FATAL] Corpus cannot be found at defined ",
             "location. Aborting...")
      }

      private$file.path <- filepath
      dt.size <- (file.info(filepath)$size / 2^30)

      message("[", class(self)[1], "][INFO] Dataset size: ",
              round(dt.size, digits = 4), " Gb.")

      if (dt.size < 1) {
        stop("[", class(self)[1], "][FATAL] Low Dimensional Dataset is not ",
             "compatible with HDDataset class loader. Aborting...")
      }

      message("[", class(self)[1], "][INFO] Loading High Dimensional Dataset...")

      if (isTRUE(header)) {
        column.names <- unlist(strsplit(scan(file = filepath, nlines = 1,
                                             what = "character", quiet = TRUE),
                                        split = sep))
        num.columns <- length(column.names)

        if (isTRUE(normalize.names))
          column.names <- make.names(column.names, unique = TRUE)

        private$corpus <- setNames(data.frame(matrix(ncol = num.columns,
                                                     nrow = 0)),
                                   column.names)
      } else {
        num.columns <- length(unlist(strsplit(scan(file = filepath, nlines = 1,
                                                   what = "character", quiet = TRUE),
                                              split = sep)))
        private$corpus <- data.frame(matrix(ncol = num.columns, nrow = 0))
      }

      private$sep <- sep
      private$start.at <- skip

      message("[", class(self)[1], "][INFO] Finish!")
    },
    #'
    #' @description get the name of the columns comprising the dataset
    #'
    #' @return a character vector with the name of each column.
    #'
    getFeatureNames = function() { names(private$corpus) },
    #'
    #' @description obtains the number of columns present in the Dataset.
    #'
    #' @return an \link{integer} of length 1 or \link{NULL}
    #'
    getNcol = function() { ncol(private$corpus) },
    #'
    #' @description create a blinded \link{HDSubset} for classification purposes.
    #'
    #' @param column.id an integer or character indicating the column (number or name
    #' respectively) identifier. Default \link{NULL} value
    #' is valid ignores defining a identification column.
    #' @param chunk.size an integer value indicating the size of chunks taken
    #' over each iteration.
    #'
    #' @return a \link{HDSubset} object.
    #'
    #' @importFrom dplyr between
    #'
    createSubset = function(column.id = FALSE, chunk.size = 100000) {
      if (!all.equal(chunk.size, as.integer(chunk.size))) {
        message("[", class(self)[1], "][WARNING] Chunk size is not valid. Must ",
                "be an integer higher than 0. Asumming 100000 as default value")
        chunk.size <- 100000
      }

      if (is.numeric(column.id) && !dplyr::between(column.id, 0, self$getNcol())) {
        message("[", class(self)[1], "][WARNING] Identifier cannot exceed number ",
                "of columns [1-", self$getNcol(), "]. Assuming no identifier")
        column.id <- FALSE
      }

      HDSubset$new(file.path = private$file.path, feature.names = private$corpus,
                   feature.id = column.id, start.at = private$start.at,
                   sep = private$sep, chunk.size = chunk.size)
    }
  ),
  private = list(
    file.path = NULL,
    corpus = NULL,
    start.at = 0,
    sep = ","
  )
)
