#' @title Plotting feature clusters following bi-class problem.
#'
#' @docType class
#'
#' @format NULL
#'
#' @details <<details>
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
    #' @description empty function used to initalize the object arguments in runtime.
    #' @return a \link{BinaryPlot} object.
    #'
    initialize = function() { },
    #'
    #' @description plots feature-clustering data from a bi-class problem.
    #'
    #' @param summary a \link{data.frame} comprising the elements to be plotted.
    #' @param ... further arguments passed down to \code{plot} function.
    #' @seealso for more information see \code{plot} function from \code{GenericPlot} class.
    #' @import ggplot2
    #'
    plot = function(summary, ...) {
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
