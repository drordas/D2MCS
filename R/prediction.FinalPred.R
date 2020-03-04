#' @title Stores the prediction for a specific voting scheme.
#'
#' @description The class is used to store the computed probability after executing an specific voting scheme.
#'
#' @docType class
#'
#' @seealso \code{\link{Prediction}} \code{\link{SimpleVoting}} \code{\link{SingleVoting}}
#' \code{\link{CombinedVoting}} \code{\link{VotingStrategy}}
#'
#' @keywords internal
#'
#' @import R6

FinalPred <- R6::R6Class(
  classname = "FinalPred",
  portable = TRUE,
  public = list(
    #'
    #' @description <<description>>
    #'
    initialize = function() {
      private$prob <- NULL
      private$raw <- NULL
      private$positive.class <- NULL
      private$negative.class <- NULL
    },
    #'
    #' @description <<description>>
    #'
    #' @param prob <<description>>
    #' @param raw <<description>>
    #' @param class.values <<description>>
    #' @param positive.class <<description>>
    #'
    #' @return <<description>>
    #'
    set = function(prob, raw, class.values, positive.class) {
      if (length(positive.class) != 1 || !(positive.class %in% class.values)) {
        stop("[", class(self)[1], "][FATAL] Positive class is invalid. ",
             "Must be one of (", paste0(class.values, collapse = ", "),
             "). Aborting...")
      }

      if (any(is.null(prob), is.null(raw), nrow(prob) == 0,
             ncol(row) == 0, length(raw) == 0)) {
        stop("[", class(self)[1], "][FATAL] Predictions were not computed. ",
             "Aborting...")
      }

      private$negative.class <- setdiff(class.values, positive.class)
      private$positive.class <- positive.class

      private$prob <- prob

      if (!is.factor(raw)) {
        private$raw <- factor(raw, levels = union(private$positive.class,
                                                  private$negative.class))
        private$raw <- relevel(private$raw, ref = private$positive.class)
      } else {
        private$raw <- raw
        private$prob <- prob
      }

      if (any(is.na(private$raw))) {
        stop("[", class(self)[1], "][FATAL] Class values contains NA's. ",
             "Aborting...")
      }

      names(private$prob) <- self$getClassValues()

    },
    #'
    #' @description <<description>>
    #'
    #' @return <<description>>
    #'
    getProb = function() { private$prob },
    #'
    #' @description <<description>>
    #'
    #' @return <<description>>
    #'
    getRaw = function() { private$raw },
    #'
    #' @description <<description>>
    #'
    #' @return <<description>>
    #'
    getClassValues = function() {
      union(private$positive.class, private$negative.class)
    },
    #'
    #' @description <<description>>
    #'
    #' @return <<description>>
    #'
    getPositiveClass = function() { private$positive.class },
    #'
    #' @description <<description>>
    #'
    #' @return <<description>>
    #'
    getNegativeClass = function() { private$negative.class }
  ),
  private = list(
    prob = NULL,
    raw = NULL,
    positive.class = NULL,
    negative.class = NULL
  )
)
