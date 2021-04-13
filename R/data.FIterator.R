#' @title Iterator over a file.
#'
#' @description Creates a \code{\link{FIterator}} object to iterate over high
#' dimensional files.
#'
#' @details Use \code{\link{HDDataset}} class to ensure the creation of a valid
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
    #' @description Method for initializing the object arguments during runtime.
    #'
    #' @param config.params A \link{list} of configuration options.
    #' @param chunk.size An \link{integer} value indicating the size of chunks
    #' taken over each iteration. By default \code{chunk.size} is defined as
    #' 10000.
    #' @param verbose A \link{logical} value to specify if more verbosity is
    #' needed.
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
    #' @description Gets the next chunk of data. Each iteration returns the same
    #' instances (data.frame rows) as chunk.size. However, if remaining data if
    #' less than chunk size, all the remaining data is returned. Conversely,
    #' \link{NULL} when there is no more pending data. By default
    #' \code{chunk.size} is defined as 10000.
    #'
    #' @return A \link{data.frame} of \link{NULL} if all the data have been
    #' previously returned.
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
        message("[", class(self)[1], "][INFO] Read lines ",
                initial, " to ", initial + private$chunk.size,
                " [", format(private$chunk.size, scientific = FALSE), "]")
      }
      private$start <- 0
      private$index <- private$index + 1
      private$read.chunk <- nrow(data.chunk)
      data.chunk
    },
    #'
    #' @description Checks if the \code{\link{FIterator}} object reached the end
    #' of the \link{data.frame}
    #'
    #' @return A \link{logical} value indicating if the end of \link{data.frame}
    #' has been reached.
    #'
    isLast = function() { private$read.chunk != private$chunk.size },
    #'
    #' @description Destroys the \code{\link{FIterator}} object.
    #'
    finalize = function() {
      if (!is.null(private$con) && isOpen(private$con)) {
        close(private$con)
        private$con <- NULL
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
