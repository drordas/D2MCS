#' @title <<tittle>>
#'
#' @description Subset
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
#' @export Subset

Subset <- R6::R6Class(
  classname = "Subset",
  portable = TRUE,
  cloneable = FALSE,
  public = list(
    #'
    #' @description <<description>>
    #'
    #' @param dataset <<description>>
    #' @param class.index <<description>>
    #' @param class.values <<description>>
    #' @param positive.class <<description>>
    #' @param feature.id <<description>>
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
    #' @description <<description>>
    #'
    #' @return <<return>>
    #'
    getFeatureNames = function() { private$feature.names },
    #'
    #' @description <<description>>
    #'
    #' @param feature.names <<description>>
    #'
    #' @return <<description>>
    #'
    getFeatures = function(feature.names = NULL) {
      if (is.vector(feature.names) && length(feature.names) > 0) {
        private$data[, intersect(names(private$data[, -private$class.index]), feature.names)]
      } else { private$data[, -private$class.index] }
    },
    #'
    #' @description <<description>>
    #'
    #' @return <<return>>
    #'
    getID = function() {
      if (!is.null(private$feature.id))
        private$feature.names[private$feature.id]
      else private$feature.id
    },
    #'
    #' @description <<description>>
    #'
    #' @param chunk.size <<description>>
    #' @param verbose <<description>>
    #'
    #' @return <<description>>
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
    #' @description <<description>>
    #'
    #' @return <<return>>
    #'
    getClassValues = function() { private$data[, private$class.index] },
    #'
    #' @description <<description>>
    #'
    #' @param target.value <<description>>
    #'
    #' @return <<return>>
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
    #' @description <<description>>
    #'
    #' @return <<return>>
    #'
    getClassIndex = function() { private$class.index },
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
    getNcol = function() { ncol(private$data) },
    #'
    #' @description <<description>>
    #'
    #' @return <<return>>
    #'
    getNrow = function() { nrow(private$data) },
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
