#' @title <<tittle>>
#'
#' @description BinaryPlot
#'
#' @docType class
#'
#' @format NULL
#'
#' @details <<details>
#'
#' @seealso \code{\link{GenericPlot}}
#'
#' @keywords NULL
#'
#' @import R6
#'
#' @export BinaryPlot

BinaryPlot <- R6::R6Class(
  classname = "BinaryPlot",
  inherit = GenericPlot,
  portable = TRUE,
  public = list(
    #' @description
    initialize = function() { },
    #'
    #' @description <<description>>
    #'
    #' @param summary <<description>>
    #' @param ... <<description>>
    #'
    #' @return <<description>>
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
