#' @title <<tittle>>
#'
#' @description DefaultModelFit
#'
#' @docType class
#'
#' @format NULL
#'
#' @details <<details>
#'
#' @seealso \code{\link{GenericModelFit}}
#'
#' @keywords NULL
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
    #' @description <<description>>
    #'
    #' @param instances <<description>
    #' @param class.name <<description>>
    #'
    initialize = function(instances, class.name) {
      super$initialize(instances, class.name)
    },
    #'
    #' @description <<description>>
    #'
    #' @param simplify <<description>>
    #'
    #' @return <<description>>
    #'
    createFormula = function(simplify = FALSE) {
      if (isTRUE(simplify))
        as.formula(paste0(sprintf("`%s`", private$class.name), " ~ ."))
      else as.formula(paste0(paste0(sprintf("`%s`", private$class.name), " ~ "),
                              paste0(sprintf("`%s`", private$feature.names), collapse = "+")))
    },
    #'
    #' @description <<description>>
    #'
    #' @return <<description>>
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
