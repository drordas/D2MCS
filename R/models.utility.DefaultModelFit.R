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
    #' @return A \code{\link{DefaultModelFit}} object.
    #'
    initialize = function() { super$initialize() },
    #'
    #' @description The function is responsible of creating a \code{\link{formula}} for M.L. model.
    #'
    #' @param instances A \code{\link{data.frame}} containing the instances
    #' used to create the recipe.
    #' @param class.name A \code{\link{character}} vector representing
    #' the name of the target class.
    #' @param simplify A \code{\link{logical}} argument defining whether the
    #' formula should be generated as simple as posible.
    #'
    #' @return A \code{\link{formula}} object.
    #'
    createFormula = function(instances, class.name, simplify = FALSE) {
      if(isTRUE(simplify))
        as.formula( paste0(sprintf("`%s`", class.name )," ~ .") )
      else as.formula(paste0( paste0(sprintf("`%s`", class.name )," ~ "),
                              paste0(sprintf("`%s`",names(instances) ),
                                     collapse = "+" )) )
    },
    #'
    #' @description The function is responsible of creating a
    #' \code{\link[recipes]{recipe}} with five operations over the data:
    #' \code{\link[recipes]{step_zv}}, \code{\link[recipes]{step_nzv}},
    #' \code{\link[recipes]{step_corr}},
    #' \code{\link[recipes]{step_center}}, \code{\link[recipes]{step_scale}}
    #'
    #' @details This function is automatically invoked by \code{DDMCS} during
    #' model training stage. If
    #'
    #' @param instances A \code{\link{data.frame}} containing the instances
    #' used to create the recipe.
    #' @param class.name A \code{\link{character}} vector representing
    #' the name of the target class.
    #' @return A object of class \code{\link[recipes]{recipe}}.
    #'
    #' @import recipes
    #'
    createRecipe = function(instances, class.name) {
      recipe <- recipe( self$createFormula(instances, class.name,
                                           simplify = TRUE),
                        data = instances )
      recipe %>% step_zv( all_predictors() ) %>%
        step_nzv( all_predictors() ) %>%
        step_corr( all_predictors() ) %>%
        step_center( all_predictors() ) %>%
        step_scale( all_predictors() )
    }
  )
)
