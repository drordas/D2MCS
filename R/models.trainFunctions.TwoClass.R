#' @title <<tittle>>
#'
#' @description TwoClass
#'
#' @docType class
#'
#' @format NULL
#'
#' @details <<details>
#'
#' @seealso \code{\link{TrainFunction}}
#'
#' @keywords NULL
#'
#' @import R6
#'
#' @export TwoClass

TwoClass <- R6::R6Class(
  classname = "TwoClass",
  portable = TRUE,
  inherit = TrainFunction,
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
                          allowParallel, verboseIter, seed = NULL) {

      super$initialize(method, number, savePredictions, classProbs,
                       allowParallel, verboseIter, seed)
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
    #' @import caret
    #'
    create = function(summaryFunction, search.method = "grid", class.probs = NULL) {
      if (is.null(summaryFunction) ||  !"SummaryFunction" %in% class(summaryFunction))
        stop("[", class(self)[1], "][FATAL] SummaryFunction parameter must be ",
             "defined as 'SummaryFunction' type. Aborting...")
      else {
        private$summaryFunction <- summaryFunction$execute
        if (search.method %in% c("grid", "random")) {
          private$search <- search.method
        } else {
          message("[", class(self)[1], "][WARNING] Invalid search method. ",
                  "Only 'random' or 'grid' search method are available. ",
                  "Assuming grid method")

        }
        class.probability <- ifelse((!is.null(class.probs) &&
                                         is.logical(class.probs)),
                                     class.probs,  super$getClassProbs())

        private$trFunction <- caret::trainControl(method = super$getResamplingMethod(),
                                                   number = super$getNumberFolds(),
                                                   savePredictions = super$getSavePredictions(),
                                                   classProbs = class.probability,
                                                   summaryFunction = private$summaryFunction,
                                                   search = private$search,
                                                   allowParallel = super$getAllowParallel(),
                                                   verboseIter = super$getVerboseIter())

        private$measures <- summaryFunction$getMeasures()
      }
    },
    #'
    #' @description <<description>
    #'
    #' @return <<description>>
    #'
    getTrFunction = function() {
      if (is.null(private$trFunction))
        message("[", class(self)[1], "][WARNING] TrainFunction is not created. ",
                "Execute create method first. Task not performed")
      private$trFunction
    },
    #'
    #' @description <<description>
    #'
    #' @param class.probs <<description>>
    #'
    #' @return <<description>>
    #'
    setClassProbs = function(class.probs) {
      if (is.null(class.probs) || !is.logical(class.probs))
        message("[", class(self)[1], "][WARNING] Class probabilities parameter ",
                "is null or erroneous. Task not performed")
      else {
        if (is.null(private$trFunction)) {
          private$classProbs <- class.probs
          if (!is.null(private$summaryFunction))
            self$create(summaryFunction, private$search)
          else message("[", class(self)[1], "][WARNING] SummaryFunction parameter ",
                       "is not defined. Unable to create TrainFunction. Task not performed")
        } else private$trFunction$classProbs <- class.probs
      }
    },
    #'
    #' @description <<description>
    #'
    #' @return <<description>>
    #'
    getMeasures = function() { private$measures },
    #'
    #' @description <<description>
    #'
    #' @return <<description>>
    #'
    getType = function() { private$type },
    #'
    #' @description <<description>
    #'
    #' @param summaryFunction <<description>>
    #'
    #' @return <<description>>
    #'
    setSummaryFunction = function(summaryFunction) {
      if (is.null(summaryFunction) || !inherit(summaryFunction, "SummaryFunction")) {
        message("[", class(self)[1], "]][WARNING] SummaryFunction parameter ",
                "is null or incorrect type. Task not performed")
      } else {
        if (is.null(private$trFunction)) {
          self$create(private$summaryFunction, private$search)
        } else private$trFunction$summaryFunction <- summaryFunction$execute
      }
    }
  ),
  private = list(
    measures = NULL,
    search = "grid",
    trFunction = NULL,
    summaryFunction = NULL,
    type = "Bi-Class"
  )
)
