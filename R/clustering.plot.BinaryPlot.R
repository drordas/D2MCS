#' @title Plotting feature clusters following bi-class problem.
#'
#' @description The \code{\link{BinaryPlot}} implements a basic plot for
#' bi-class problem.
#'
#' @seealso \code{\link{GenericPlot}}
#'
#' @keywords device color hplot
#'
#' @import R6
#'
#' @export BinaryPlot

BinaryPlot <- R6::R6Class(
  classname = "BinaryPlot",
  inherit = GenericPlot,
  portable = TRUE,
  public = list(
    #'
    #' @description Empty function used to initialize the object arguments in
    #' runtime.
    #'
    initialize = function() { },
    #'
    #' @description Plots feature-clustering data from a bi-class problem.
    #'
    #' @param summary A \link{data.frame} comprising the elements to be plotted.
    #'
    #' @import ggplot2
    #'
    plot = function(summary) {
      if (!is.data.frame(summary)) {
        stop("[", class(self)[1], "][FATAL] Summary parameter must be defined ",
             "as 'data.frame' type. Aborting...")
      }
      super$plot(summary) + ggplot2::labs(title = "Binary Data") +
        ggplot2::theme_light() +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 0.5))
    }
  )
)
