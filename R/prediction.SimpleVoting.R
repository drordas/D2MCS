#' @title Abtract class to define simple voting schemes.
#'
#' @description Abstract class used as a template to define new customized simple voting schemes.
#'
#' @docType class
#'
#' @format NULL
#'
#' @seealso \code{\link{DDMCS}}, \code{\link{ClassMajorityVoting}}, \code{\link{ClassWeightedVoting}},
#' \code{\link{ProbAverageVoting}}, \code{\link{ProbAverageWeightedVoting}}, \code{\link{ProbBasedMethodology}},
#' \code{\link{CombinedVoting}}
#'
#' @keywords models methods math
#'
#' @import R6
#'
#' @export SimpleVoting

SimpleVoting <- R6::R6Class(
  classname = "SimpleVoting",
  portable = TRUE,
  public = list(
    #'
    #' @description Method for initializing the object arguments during runtime.
    #'
    #' @param cutoff A \code{\link{character}} vector defining the minimum probability used to perform a
    #' a positive classification. If is not defined, 0.5 will be used as default value.
    #'
    initialize = function(cutoff = NULL) {
      if (!is.null(cutoff) && !is.numeric(cutoff)) {
        stop("[", class(self)[1], "][FATAL] Invalid values of cutoff. Aborting...")
      }

      if (is.null(cutoff) || !is.numeric(cutoff) || !(dplyr::between(cutoff, 0, 1))) {
        private$cutoff <- 0.5
      } else private$cutoff <- cutoff

      private$final.pred <- FinalPred$new()
    },
    #'
    #' @description The function obtains the minimum probabilistic value used to perform a positive classification.
    #'
    #' @return A \code{\link{numeric}} value.
    #'
    getCutoff = function() { private$cutoff },
    #'
    #' @description The function is used to return the prediction values computed by a voting stratety.
    #'
    #' @param type A \code{\link{character}} to define which type of predictions should be returned. If not defined
    #' all type of probabilities will be returned. Conversely if 'prob' or 'raw' is defined then computed 'probabilistic' or
    #' 'class' values are returned.
    #' @param target A \code{\link{character}} defining the value of the positive class.
    #' @param filter A \code{\link{logical}} value used to specify if only predictions
    #' matching the target value should be returned or not. If \code{\link{TRUE}} the function returns only the
    #' predictions matching the target value. Conversely if \code{\link{FALSE}} (by default)
    #' the function returns all the predictions.
    #'
    #' @return A \code{\link{FinalPred}} object.
    #'
    getFinalPred = function(type = NULL, target = NULL, filter = NULL) {
      if (any(is.null(type), !(type %in% c("raw", "prob")))) {
        private$final.pred
      } else {
        if (!is.logical(filter)) {
          message("[", class(self)[1], "][WARNING] Filter parameter must be ",
                  "defined as 'logical' type. Aborting...")
          filter <- FALSE
        }
        class.values <- private$final.pred$getClassValues()

        switch(type,
               "prob" = {
                 if (is.null(target) || !(target %in% class.values)) {
                   message("[", class(self)[1], "][WARNING] Target not ",
                           "specified or invalid. Using '",
                           paste0(private$final.pred$getClassValues(),
                                   collapse = ", "), "'")
                   target <- private$final.pred$getClassValues()
                 }
                 if (filter) {
                   private$final.pred$getProb()[private$final.pred$getRaw() == target,
                                                target, drop = FALSE]
                 } else {
                   private$final.pred$getProb()[, target, drop = FALSE]
                 }
               },
               "raw" = {
                 if (filter) {
                   private$final.pred$getRaw()[private$final.pred$getRaw() == target,
                                               , drop = FALSE]
                 } else { private$final.pred$getRaw() }
               }
        )
      }
    },
    #'
    #' @description Abstract function used to implement the operation of the voting scheme.
    #'
    #' @param predictions A \code{\link{ClusterPredictions}} object containing all the predictions achieved for each cluster.
    #' @param verbose A \code{\link{logical}} value to specify if more verbosity is needed.
    #'
    execute = function(predictions, verbose = FALSE) {
      stop("[", class(self)[1], "][FATAL] Class is abstract. ",
           "Method should be defined in inherited class. Aborting...")
    }
  ),
  private = list(
    cutoff = NULL,
    final.pred = NULL
  )
)
