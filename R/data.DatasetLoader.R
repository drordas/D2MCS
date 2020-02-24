#' @title <<tittle>>
#'
#' @description DatasetLoader
#'
#' @docType class
#'
#' @format NULL
#'
#' @details <<details>
#'
#' @seealso \code{\link{Dataset}}, \code{\link{HDDataset}}
#'
#' @keywords NULL
#'
#' @import R6
#'
#' @export DatasetLoader

DatasetLoader <- R6::R6Class(
  classname = "DatasetLoader",
  portable = TRUE,
  cloneable = FALSE,
  public = list(
    #'
    #' @description <<description>>
    #'
    #' @return <<return>>
    #'
    initialize = function() { self },
    #'
    #' @description <<description>>
    #'
    #' @param filepath <<description>>
    #' @param header <<description>>
    #' @param sep <<description>>
    #' @param skip.lines <<description>>
    #' @param target.class <<description>>
    #' @param positive.class <<description>>
    #' @param normalize.names <<description>>
    #' @param string.as.factor <<description>>
    #' @param ignore.columns <<description>>
    #'
    #' @return <<descriptionReturn>>
    #'
    #' @importFrom dplyr between
    #'
    load = function(filepath, header = TRUE, sep = ",", skip.lines = 0,
                    target.class = NULL, positive.class = NULL,
                    normalize.names = FALSE, string.as.factor = FALSE,
                    ignore.columns = NULL ){

      if (is.null(filepath) || !file.exists(filepath)) {
        stop("[",class(self)[1],"][FATAL] Corpus cannot be found at defined ",
             "location. Aborting...")
      }

      dt.size <- (file.info(filepath)$size / 2^30)

      if ( dplyr::between(dt.size,0,1) ){

        if (is.null(positive.class)){
          stop("[",class(self)[1],"][FATAL] Positive class was not defined. ",
               "Aborting...")
        }

        if( !inherits(target.class, c("character","numeric") )){
          stop("[",class(self)[1],"][FATAL] Target class parameter must be ",
               "defined as 'numerical' or 'character' type. Aborting...")
        }else{
          if ( is.character(target.class) && !isTRUE(header)) {
            stop("[",class(self)[1],"][FATAL] Cannot name target class ",
                 "without columns names. Aborting...")
          }
        }

        dataset <- Dataset$new(filepath = filepath, header = header, sep = sep,
                               skip = skip.lines, normalize.names = normalize.names,
                               target.class = target.class,  positive.class = positive.class,
                               ignore.columns = ignore.columns)
      }else{
        dataset <- HDDataset$new(filepath = filepath, header = header, sep = sep,
                                 skip = skip.lines, normalize.names = normalize.names,
                                 ignore.columns = ignore.columns)
      }
      dataset
    }
  )
)
