#' @title Abstract class for defining model fitting method.
#'
#' @description Template to create a \code{\link[recipes]{recipe}} or \code{\link{formula}} objects used in model training stage.
#'
#' @docType class
#'
#' @seealso \code{\link{DefaultModelFit}}, \code{\link[caret]{train}}
#'
#' @keywords misc
#'
#' @import R6
#'
#' @export GenericModelFit

GenericModelFit <- R6::R6Class(
  classname = "GenericModelFit",
  portable = TRUE,
  public = list(
    #'
    #' @description Method for initializing the object arguments during runtime.
    #'
    #' @param instances An \code{\link{data.frame}} containing the data used to train the M.L. models.
    #' @param class.name A \code{\link{character}} of length 1 defining the name of the target class.
    #'
    initialize = function(instances, class.name) {
      if (any(!"data.frame" %in% class(instances), nrow(instances) == 0)) {
        stop("[", class(self)[1], "][FATAL] Instances must be a non-empty data.frame. ",
             "Aborting...")
      }

      if (!(class.name %in% names(instances))) {
        stop("[", class(self)[1], "][FATAL] Class name not included in instances ",
             "data.frame. Aborting...")
      }

      private$class.name <- class.name
      private$feature.names <- names(instances)
      private$instances <- instances
    },
    #'
    #' @description The function is responsible of creating a \code{\link{formula}} for M.L. model.
    #'
    #' @param simplify A \code{\link{logical}} argument defining whether the formula should be generated as simple as posible.
    #'
    #' @return A \code{\link{formula}} object.
    #'
    createFormula = function(simplify = TRUE) {
      stop("[", class(self)[1], "][FATAL] Class is abstract. ",
           "Method should be defined in inherited class. Aborting...")
    },
    #'
    #' @description The function is responsible of creating a \code{\link[recipes]{recipe}} for M.L. model.
    #'
    #' @return A object of class \code{\link[recipes]{recipe}}.
    #'
    createRecipe = function() {
      stop("[", class(self)[1], "][FATAL] Class is abstract. ",
           "Method should be defined in inherited class. Aborting...")
    }
  ),
  private = list(
    class.name = NULL,
    feature.names = NULL,
    instances = NULL
  )
)
