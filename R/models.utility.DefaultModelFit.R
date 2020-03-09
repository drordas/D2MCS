#' @title Default model fitting implementation.
#'
#' @description Creates a default \code{\link[recipes]{recipe}} and \code{\link{formula}} objects used in model training stage.
#'
#' @seealso \code{\link{GenericModelFit}}, \code{\link[caret]{train}}
#'
#' @keywords misc
#'
#' @import R6
#'
#' @export DefaultModelFit

DefaultModelFit <- R6::R6Class(
  classname = "DefaultModelFit",
  inherit = GenericModelFit,
  portable = TRUE,
  public = list(
    #'
    #' @description Method for initializing the object arguments during runtime.
    #'
    #' @param instances An \code{\link{data.frame}} containing the data used to train the M.L. models.
    #' @param class.name A \code{\link{character}} of length 1 defining the name of the target class.
    #'
    initialize = function(instances, class.name) {
      super$initialize(instances, class.name)
    },
    #'
    #' @description The function is responsible of creating a \code{\link{formula}} for M.L. model.
    #'
    #' @param simplify A \code{\link{logical}} argument defining whether the formula should be generated as simple as posible.
    #'
    #' @return A \code{\link{formula}} object.
    #'
    createFormula = function(simplify = FALSE) {
      if (isTRUE(simplify))
        as.formula(paste0(sprintf("`%s`", private$class.name), " ~ ."))
      else as.formula(paste0(paste0(sprintf("`%s`", private$class.name), " ~ "),
                             paste0(sprintf("`%s`", private$feature.names), collapse = "+")))
    },
    #'
    #' @description The function is responsible of creating a \code{\link[recipes]{recipe}} with five operations over the data:
    #' \code{\link[recipes]{step_zv}}, \code{\link[recipes]{step_nzv}}, \code{\link[recipes]{step_corr}},
    #' \code{\link[recipes]{step_center}}, \code{\link[recipes]{step_scale}}
    #'
    #' @return A object of class \code{\link[recipes]{recipe}}.
    #'
    #' @import recipes
    #'
    createRecipe = function() {
      recipe <- recipes::recipe(self$createFormula(simplify = TRUE), data = private$instances)
      recipe %>% recipes::step_zv(recipes::all_predictors()) %>%  recipes::step_nzv(recipes::all_predictors()) %>%
        recipes::step_corr(recipes::all_predictors()) %>% recipes::step_center(recipes::all_predictors()) %>%
        recipes::step_scale(recipes::all_predictors())
    }
  )
)
