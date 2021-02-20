#' @title Compute performance across resamples.
#'
#' @description Computes the performance across resamples when class
#' probabilities can be computed.
#'
#' @seealso \code{\link{SummaryFunction}}
#'
#' @keywords misc
#'
#' @import R6
#'
#' @export UseProbability

UseProbability <- R6::R6Class(
  classname = "UseProbability",
  inherit = SummaryFunction,
  portable = TRUE,
  public = list(
    #'
    #' @description The function defined during runtime the usage of seven
    #' measures: 'ROC', 'Sens', 'Kappa', 'Accuracy', 'TCR_9', 'MCC' and 'PPV'.
    #'
    initialize = function() {
      super$initialize(c("ROC", "Sens", "Spec", "Kappa", "Accuracy", "TCR_9", "MCC", "PPV"))
    },
    #'
    #' @description The function computes the performance across resamples using
    #' the previously defined measures.
    #'
    #' @param data A \link{data.frame} containing the data used to compute the
    #' performance.
    #' @param lev An optional value used to define the levels of the target
    #' class.
    #' @param model An optional value used to define the M.L. model used.
    #'
    #' @return A vector of performance estimates.
    #'
    #' @import caret
    #' @importFrom mltools mcc
    #' @importFrom ModelMetrics auc
    #'
    execute = function(data, lev = NULL, model = NULL) {
      lvls <- levels(data$obs)
      if (length(lvls) > 2)
        stop("[", class(self)[1], "][FATAL] Your outcome has ", length(lvls),
             " levels. The 'UseProbability' function is not appropriate. Aborting...")
      if (!all(levels(data[, "pred"]) == lvls))
        stop("[", class(self)[1], "][FATAL] Levels of observed and predicted data ",
             "do not match. Aborting...")

      data$y = as.numeric(data$obs == lvls[2])
      data$z = as.numeric(data$pred == lvls[2])
      rocAUC <- ModelMetrics::auc(ifelse(data$obs == lev[2], 0, 1), data[, lvls[1]])
      confMat <- caret::confusionMatrix(table(data$z, data$y), positive = "1")
      mcc <- mltools::mcc(TP = confMat$table[1, 1], FP = confMat$table[1, 2], TN = confMat$table[2, 2], FN = confMat$table[2, 1])
      ppv <- (confMat$table[1, 1] / (confMat$table[1, 1] + confMat$table[1, 2]))
      fn_tcr_9 <- (9 * confMat$table[1, 2] + confMat$table[2, 1]) / (9 * (confMat$table[1, 2] + confMat$table[2, 2]) +
                                                                    confMat$table[2, 1] + confMat$table[1, 1])
      out <- c(rocAUC,
               caret::sensitivity(data[, "pred"], data[, "obs"], lev[1]),
               caret::specificity(data[, "pred"], data[, "obs"], lev[2]),
               confMat$overall['Kappa'],
               confMat$overall['Accuracy'],
               fn_tcr_9, mcc, ppv)
      names(out) <- c("ROC", "Sens", "Spec", "Kappa", "Accuracy", "TCR_9", "MCC", "PPV")
      out
    }
  )
)
