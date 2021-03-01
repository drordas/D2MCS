#' @title Iterator over a Subset object
#'
#' @description Creates a \code{\link{DIterator}} object to iterate over the
#' \code{\link{Subset}}.
#'
#' @seealso \code{\link{Dataset}}
#'
#' @keywords internal manip datagen
#'
#' @import R6
#'
#' @export DIterator

DIterator <- R6::R6Class(
  classname = "DIterator",
  portable = TRUE,
  public = list(
    #'
    #' @description Method for initializing the object arguments during runtime.
    #'
    #' @param data A \link{data.frame} structure to be iterated.
    #' @param chunk.size An \link{integer} value indicating the size of chunks
    #' taken over each iteration. By default \code{chunk.size} is defined as
    #' 10000.
    #' @param verbose A \link{logical} value to specify if more verbosity is
    #' needed.
    #'
    initialize = function(data, chunk.size, verbose) {
      private$chunk.size  <- chunk.size
      private$read.chunk  <- chunk.size
      private$verbose <- verbose
      private$start <- 0
      private$end <- 0
      private$data <- data
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
      if (self$isLast()) return(NULL)
      if ((private$start + private$chunk.size) > nrow(private$data)) {
        private$end <- private$start + (nrow(private$data) - private$start)
        private$read.chunk <- seq(private$start, private$end)
      } else {
        private$end <- (private$start + private$chunk.size)
        private$read.chunk <- seq(private$start, private$end)
      }

      data.chunk <- private$data[private$read.chunk, ]

      if (isTRUE(private$verbose)) {
        message("[", class(self)[1], "][INFO] Read lines ",
                private$start, " to ", private$end,
                " [", format(private$end - private$start, scientific = FALSE), "]")
      }
      private$start <- private$end + 1
      data.chunk
    },
    #'
    #' @description Checks if the \code{\link{DIterator}} object reached the end
    #' of the \link{data.frame}
    #'
    #' @return A \link{logical} value indicating if the end of \link{data.frame}
    #' has been reached.
    #'
    isLast = function() { private$end >= nrow(private$data) },
    #'
    #' @description Destroys the \code{\link{DIterator}} object.
    #'
    finalize = function() {}
  ),
  private = list(
    chunk.size = NULL,
    verbose = FALSE,
    start = NULL,
    end = 0,
    read.chunk = 0,
    data = 0
  )
)
