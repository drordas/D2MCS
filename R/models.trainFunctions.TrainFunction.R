#' @title <<tittle>>
#'
#' @description TrainFunction
#'
#' @docType class
#'
#' @format NULL
#'
#' @details <<details>
#'
#' @seealso \code{\link{TwoClass}}
#'
#' @keywords NULL
#'
#' @import R6
#'
#' @export TrainFunction

TrainFunction <- R6::R6Class(
  classname = "TrainFunction",
  portable = TRUE,
  public = list(
    #'
    #' @description <<description>>
    #'
    #' @param method <<description>>
    #' @param number <<description>>
    #' @param savePredictions <<description>>
    #' @param classProbs <<description>>
    #' @param allowParallel <<description>>
    #' @param verboseIter <<description>>
    #' @param seed <<description>>
    #'
    initialize = function(method, number, savePredictions, classProbs,
                          allowParallel, verboseIter, seed) {
      private$method <- method
      private$folds <- number
      private$savePredictions <- savePredictions
      private$classProbs <- classProbs
      private$allowParallel <- allowParallel
      private$verboseIter <- verboseIter
      if (!is.numeric(seed)) {
        private$seed <- .Random.seed[ceiling(runif(1, 0, length(.Random.seed)))]
        message("[", class(self)[1], "][INFO] Using random seed '", private$seed, "'")
      } else {
        private$seed <- seed
        message("[", class(self)[1], "][INFO] Using static seed '", private$seed, "'")
      }
    },
    #'
    #' @description <<description>
    #'
    #' @param summaryFunction <<description>>
    #' @param search.method <<description>>
    #' @param class.probs <<description>>
    #'
    #' @return <<description>>
    #'
    create = function(summaryFunction, search.method = "grid", class.probs) {
      stop("[", class(self)[1], "][FATAL] Class is abstract. ",
           "Method should be defined in inherited class. Aborting...")
    },
    #'
    #' @description <<description>
    #'
    #' @return <<description>>
    #'
    getResamplingMethod = function() { private$method },
    #'
    #' @description <<description>
    #'
    #' @return <<description>>
    #'
    getNumberFolds = function() { private$folds },
    #'
    #' @description <<description>
    #'
    #' @return <<description>>
    #'
    getSavePredictions = function() { private$savePredictions },
    #'
    #' @description <<description>
    #'
    #' @return <<description>>
    #'
    getClassProbs = function() { private$classProbs },
    #'
    #' @description <<description>
    #'
    #' @return <<description>>
    #'
    getAllowParallel = function() { private$allowParallel },
    #'
    #' @description <<description>
    #'
    #' @return <<description>>
    #'
    getVerboseIter = function() { private$verboseIter },
    #'
    #' @description <<description>
    #'
    #' @return <<description>>
    #'
    getTrFunction = function() {
      stop("[", class(self)[1], "][FATAL] Class is abstract. ",
           "Method should be defined in inherited class. Aborting...")
    },
    #'
    #' @description <<description>
    #'
    #' @return <<description>>
    #'
    getMeasures = function() {
      stop("[", class(self)[1], "][FATAL] Class is abstract. ",
           "Method should be defined in inherited class. Aborting...")
    },
    #'
    #' @description <<description>
    #'
    #' @return <<description>>
    #'
    getType = function() {
      stop("[", class(self)[1], "][FATAL] Class is abstract. ",
           "Method should be defined in inherited class. Aborting...")
    },
    #'
    #' @description <<description>
    #'
    #' @return <<description>>
    #'
    getSeed = function() { private$seed },
    #'
    #' @description <<description>
    #'
    #' @param summaryFunction <<description>>
    #'
    #' @return <<description>>
    #'
    setSummaryFunction = function(summaryFunction) {
      stop("[", class(self)[1], "][FATAL] Class is abstract. ",
           "Method should be defined in inherited class. Aborting...")
    },
    #'
    #' @description <<description>
    #'
    #' @param class.probs <<description>>
    #'
    #' @return <<description>>
    #'
    setClassProbs = function(class.probs) {
      stop("[", class(self)[1], "][FATAL] Class is abstract. ",
           "Method should be defined in inherited class. Aborting...")
    }
  ),
  private = list(
    method = NULL,
    folds = NULL,
    savePredictions = NULL,
    classProbs = NULL,
    allowParallel = NULL,
    verboseIter = NULL,
    seed = NULL
  )
)
