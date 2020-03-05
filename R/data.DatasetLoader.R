#' @title Dataset creation.
#'
#' @description Wrapper class able to automatically create a \code{\link{Dataset}}, \code{\link{HDDataset}} according to the input data.
#'
#' @docType class
#'
#' @format NULL
#'
#' @seealso \code{\link{Dataset}}, \code{\link{HDDataset}}
#'
#' @keywords datasets manip attribute connection file datagen
#'
#' @import R6
#'
#' @export DatasetLoader
#'
#' @examples
#' \dontrun{
#'   # Create Dataset Handler object.
#'   loader <- DatasetLoader$new()
#'
#'   #Load input file.
#'   data <- loader$load(filepath = system.file(file.path("examples","hcc-data-complete-balanced.csv"),
#'                                              package = "DDMCS"),
#'                       header = T, normalize.names = T)
#'}

DatasetLoader <- R6::R6Class(
  classname = "DatasetLoader",
  portable = TRUE,
  cloneable = FALSE,
  public = list(
    #'
    #' @description Creates the \code{\link{DatasetLoader}} object.
    #'
    #' @return DatasetLoader object.
    #'
    initialize = function() { self },
    #'
    #' @description Stores the input source into a \code{\link{Dataset}} or \code{\link{HDDataset}} type object.
    #'
    #' @param filepath the name of the file which the data are to be read from.
    #' Each row of the table appears as one line of the file.
    #' If it does not contain an _absolute_ path, the file name is _relative_ to the current working directory, ‘getwd()’.
    #' @param header a logical value indicating whether the file contains the names of the variables as its first line.
    #' If missing, the value is determined from the file format: ‘header’ is set to ‘TRUE’ if and only if the
    #' first row contains one fewer field than the number of columns.
    #' @param sep the field separator character. Values on each line of the file are separated by this character.
    #' @param skip.lines defines the number of header lines should be skipped.
    #' @param normalize.names a logical value indicating whether the columns names should be automatically renamed
    #' to ensure R compatibility.
    #' @param string.as.factor a logical value indicating if character columns should be converted to factors (default = FALSE).
    #' @param ignore.columns specify the columns from the input file that should be ignored.
    #'
    #' @return \code{\link{Dataset}}, \code{\link{HDDataset}} object.
    #' @importFrom dplyr between
    #'
    load = function(filepath, header = TRUE, sep = ",", skip.lines = 0,
                    normalize.names = FALSE, string.as.factor = FALSE,
                    ignore.columns = NULL) {

      if (is.null(filepath) || !file.exists(filepath)) {
        stop("[", class(self)[1], "][FATAL] Corpus cannot be found at defined ",
             "location. Aborting...")
      }

      dt.size <- (file.info(filepath)$size / 2^30)

      if (dplyr::between(dt.size, 0, 1)) {

        dataset <- Dataset$new(filepath = filepath, header = header, sep = sep,
                               skip = skip.lines, normalize.names = normalize.names,
                               ignore.columns = ignore.columns)
      } else {
        dataset <- HDDataset$new(filepath = filepath, header = header, sep = sep,
                                 skip = skip.lines, normalize.names = normalize.names,
                                 ignore.columns = ignore.columns)
      }
      dataset
    }
  )
)
