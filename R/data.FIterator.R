#' @title Iterator over a \link{file}.
#'
#' @description Creates an \link{FIterator} object to iterate over high dimensional files.
#'
#' @docType class
#'
#' @details Use \link{HDDataset} class to ensure the creation of a valid
#' \code{\link{FIterator}} object.
#'
#' @seealso \code{\link{Dataset}}
#'
#' @keywords internal manip connection file datagen
#'
#' @import R6
#'
#' @export FIterator

FIterator <- R6::R6Class(
  classname = "FIterator",
  portable = TRUE,
  public = list(
    #'
    #' @description Creates the \link{FIterator} object.
    #'
    #' @param config.params A \code{link{list}} of configuration options.
    #' @param chunk.size An integer value indicating the size of chunks taken
    #' over each iteration. By default chunk.size is defied as 10000.
    #' @param verbose A logical value to specify if more verbosity is needed.
    #'
    #' @return An \link{FIterator} object
    #'
    initialize = function(config.params, chunk.size, verbose) {
      private$params <- config.params
      private$chunk.size  <- chunk.size
      private$read.chunk  <- chunk.size
      private$verbose <- verbose
      private$con <- file(description = config.params$file.path, open = "r")
      private$start <- config.params$start
      private$index <- 0
    },
    #'
    #' @description Gets the next chunk of data. Each iteration returns the same instances
    #' (data.frame rows) as chunk.size. However, if remaining data if less than chunk size,
    #' all the remaining data is returned. Conversely, \link{NULL} when there is no more
    #' pending data. By default chunk.size is defied as 10000.
    #'
    #' @return A \link{data.frame} of \link{NULL} if all the data have been previously returned.
    #'
    getNext = function() {
      if (is.null(private$con) || !isOpen(private$con) || self$isLast()) {
        return(NULL)
      }
      data.chunk <- read.table(private$con,
                               nrows = private$chunk.size,
                               skip = private$start, header = FALSE,
                               sep = private$params$sep,
                               col.names = private$params$feature.names,
                               stringsAsFactors = FALSE)

      if (isTRUE(private$verbose)) {
        initial <- (private$index * private$chunk.size) + private$start
        message("[", class(self)[1], "][INFO] Readed lines ",
                initial, " to ", initial + private$chunk.size,
                " [", format(private$chunk.size, scientific = FALSE), "]")
      }
      private$start <- 0
      private$index <- private$index + 1
      private$read.chunk <- nrow(data.chunk)
      data.chunk
    },
    #'
    #' @description Checks if the \link{FIterator} object reached the end of the \link{data.frame}
    #'
    #' @return A logical value indicating if the end of \link{data.frame} has been reached.
    #'
    isLast = function() { private$read.chunk != private$chunk.size },
    #'
    #' @description Destroys the \link{FIterator} object.
    #'
    finalize = function() {
      if (!is.null(private$con) && isOpen(private$con)) {
        message("[", class(self)[1], "][INFO] Closing connection")
        close(private$con)
        private$con <- NULL
      } else {
        message("[", class(self)[1], "][INFO] Finalize")
      }
    }
  ),
  private = list(
    params = NULL,
    chunk.size = NULL,
    verbose = FALSE,
    con = NULL,
    start = NULL,
    index = 0,
    read.chunk = 0
  )
)
