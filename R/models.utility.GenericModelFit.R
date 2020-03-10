#' @title Abstract class for defining model fitting method.
#'
#' @description Template to create a \code{\link[recipes]{recipe}} or \code{\link{formula}} objects used in model training stage.
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
    #' @return A \code{\link{GenericModelFit}} object.
    #'
    initialize = function() { },
    #'
    #' @description The function is responsible of creating a
    #' \code{\link{formula}} for M.L. model.
    #'
    #' @param instances A \code{\link{data.frame}} containing the instances
    #' used to create the recipe.
    #' @param class.name A \code{\link{character}} vector representing
    #' the name of the target class.
    #' @param simplify A \code{\link{logical}} argument defining whether
    #' the formula should be generated as simple as posible.
    #'
    #' @return A \code{\link{formula}} object.
    #'
    createFormula = function(instances, class.name, simplify = TRUE) {
      stop("[", class(self)[1], "][FATAL] Class is abstract. ",
           "Method should be defined in inherited class. Aborting...")
    },
    #'
    #' @description The function is responsible of creating a
    #' \code{\link[recipes]{recipe}} for M.L. model.
    #'
    #' @param instances A \code{\link{data.frame}} containing the instances
    #' used to create the recipe.
    #' @param class.name A \code{\link{character}} vector representing
    #' the name of the target class.
    #'
    #' @return A object of class \code{\link[recipes]{recipe}}.
    #'
    createRecipe = function(instances, class.name) {
      stop("[", class(self)[1], "][FATAL] Class is abstract. ",
           "Method should be defined in inherited class. Aborting...")
    }
  ),
  private = list()
)
