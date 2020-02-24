#' @title <<tittle>>
#'
#' @description HDSubset
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
#' @export HDSubset
#'
HDSubset <- R6::R6Class(
  classname = "HDSubset",
  portable = TRUE,
  public = list(
    #'
    #' @description <<description>>
    #'
    #' @param file.path <<description>>
    #' @param feature.names <<description>>
    #' @param feature.id <<description>>
    #' @param start.at <<description>>
    #' @param sep <<description>>
    #' @param chunk.size <<description>>
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
    #' @description <<description>>
    #'
    #' @return <<return>>
    #'
    getFeatureNames = function() { private$feature.names },
    #'
    #' @description <<description>>
    #'
    #' @return <<return>>
    #'
    getNcol = function() { length(private$feature.names) },
    #'
    #' @description <<description>>
    #'
    #' @return <<return>>
    #'
    getID = function() { private$feature.names[private$feature.id] },
    #'
    #' @description <<description>>
    #'
    #' @param chunk.size <<description>>
    #' @param verbose <<description>>
    #'
    #' @return <<return>>
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
    #' @description <<description>>
    #'
    #' @return <<return>>
    #'
    finalize = function() {
      if (!is.null(private$conetion))
        close(private$conection)
      private$conection <- NULL
    },
    #'
    #' @description <<description>>
    #'
    #' @return <<return>>
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
