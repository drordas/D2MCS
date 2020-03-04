#' @title Simple Dataset handler
#'
#' @description Creates a valid simple dataset object.
#'
#' @docType class
#'
#' @format NULL
#'
#' @seealso \code{\link{HDDataset}}
#'
#' @keywords datasets manip attribute datagen
#'
#' @import R6
#'
#' @export Dataset

Dataset <- R6::R6Class(
  classname = "Dataset",
  portable = TRUE,
  cloneable = FALSE,
  public = list(
    #'
    #' @description Creates the \code{\link{Dataset}} object.
    #'
    #' @param filepath filepath the name of the file which the data are to be read from.
    #' Each row of the table appears as one line of the file.
    #' If it does not contain an _absolute_ path, the file name is _relative_ to the current working directory, ‘getwd()’.
    #' @param header a logical value indicating whether the file contains the names of the variables as its first line.
    #' If missing, the value is determined from the file format: ‘header’ is set to ‘TRUE’ if and only if the
    #' first row contains one fewer field than the number of columns.
    #' @param sep the field separator character. Values on each line of the file are separated by this character.
    #' @param skip defines the number of header lines should be skipped.
    #' @param target.class <<description>>
    #' @param positive.class <<description>>
    #' @param normalize.names a logical value indicating whether the columns names should be automatically renamed
    #' to ensure R compatibility.
    #' @param string.as.factor a logical value indicating if character columns should be converted to factors (default = FALSE).
    #' @param ignore.columns specify the columns from the input file that should be ignored.
    #'
    #' @return a \link{Dataset} object.
    #'
    #' @importFrom utils read.csv
    #'
    initialize = function(filepath, header = TRUE, sep = ",", skip = 0,
                           target.class, positive.class, normalize.names = FALSE,
                           string.as.factor = FALSE, ignore.columns = NULL) {

      if (is.null(filepath) || !file.exists(filepath)) {
        stop("[", class(self)[1], "][FATAL] Corpus cannot be found at defined ",
             "location. Aborting...")
      }

      if (is.null(target.class)) {
        stop("[", class(self)[1], "][FATAL] Target class parameter should be defined. ",
             "Aborting...")
      }

      if (is.null(positive.class)) {
        stop("[", class(self)[1], "][FATAL] Positive class parameter should be defined. ",
             "Aborting...")
      }

      if (!inherits(target.class, c("character", "numeric"))) {
        stop("[", class(self)[1], "][FATAL] Target class is incorrect. ",
             "Must contain a numerical or character value. Aborting...")
      } else {
        if (is.character(target.class) && !isTRUE(header)) {
          stop("[", class(self)[1], "][FATAL] Cannot name target class ",
               "without columns names. Aborting...")
        } else {
          class.index <- target.class
        }
      }

      dt.size <- (file.info(filepath)$size / 2^30)

      message("[", class(self)[1], "][INFO] Dataset size: ",
              round(dt.size, digits = 4), " Gb.")

      if (dt.size > 1) {
        stop("[", class(self)[1], "][FATAL] High Dimensional Dataset is not ",
             "compatible with Dataset class loader. Aborting...")
      }

      message("[", class(self)[1], "][INFO] Loading Dataset...")

      if (isTRUE(header)) {
        private$corpus <- read.csv(filepath, header = header,
                                    skip = (skip + 1), sep = sep,
                                    stringsAsFactors = string.as.factor)

        columnNames <- unlist(strsplit(scan(file = filepath, nlines = 1,
                                            what = "character", quiet = TRUE),
                                       split = sep))

        if (is.numeric(target.class)) {
          if (!(target.class %in% 1:ncol(private$corpus))) {
            stop("[", class(self)[1], "][FATAL] Class index exceeds dataset limits. ",
                 "Must be between 1 and ", ncol(private$corpus), ". Aborting...")
          } else {
            class.index <- target.class
          }
        } else {
          class.index <- which(target.class == columnNames)
          if (length(class.index) == 0) {
            stop("[", class(self)[1], "][FATAL] There are no columns named as '",
                 target.class, "'. Aborting...")
          } else {
            message("[", class(self)[1], "][INFO] Target class value found ",
                    "at position: ", class.index)
          }
        }

        if (isTRUE(normalize.names)) {
          columnNames <- make.names(columnNames, unique = TRUE)
        }
        names(private$corpus) <- columnNames
      } else {
        private$corpus <- read.csv(filepath, header = header, skip = skip,
                                   sep = sep, stringsAsFactors = string.as.factor)
        columnNames <- colnames(private$corpus)
      }

      private$class.index <- class.index
      private$class.name <- columnNames[class.index]
      private$removed.columns <- list()

      if (is.numeric(ignore.columns)) { self$removeColumns(ignore.columns) }

      if (positive.class %in% private$corpus[, private$class.index]) {
        private$positive.class <- positive.class
        private$class.values <- as.character(unique(private$corpus[, private$class.index]))
      } else {
        stop("[", class(self)[1], "][FATAL] Positive class value not found. ",
             "Aborting...")
      }

      message("[", class(self)[1], "][INFO] Load finished! Total: ",
              nrow(private$corpus), " rows and ",
              ncol(private$corpus), " columns")
      message("[", class(self)[1], "][INFO] Class values: ",
              paste0(self$getClassValues(), collapse = ", "))

    },
    #'
    #' @description get the name of the columns comprising the dataset.
    #'
    #' @return a character vector with the name of each column.
    #'
    getColumnNames = function() { names(private$corpus) },
    #'
    #' @description gets the full dataset.
    #'
    #' @return a \link{data.frame} with all the loaded information.
    #'
    getDataset = function() { private$corpus },
    #'
    #' @description <<description>>
    #'
    #' @return <<return>>
    #'
    getClassName = function() { private$class.name },
    #'
    #' @description <<description>>
    #'
    #' @return <<return>>
    #'
    getClassIndex = function() { private$class.index },
    #'
    #' @description <<description>>
    #'
    #' @return <<return>>
    #'
    getClassValues = function() { private$class.values },
    #'
    #' @description <<description>>
    #'
    #' @return <<return>>
    #'
    getPositiveClass = function() { private$positive.class },
    #'
    #' @description obtains the number of columns present in the Dataset.
    #'
    #' @return an \link{integer} of length 1 or \link{NULL}
    #'
    getNcol = function() { ncol(private$corpus) },
    #'
    #' @description obtains the number of rows present in the Dataset.
    #'
    #' @return an \link{integer} of length 1 or \link{NULL}
    #'
    getNrow = function() { nrow(private$corpus) },
    #'
    #' @description <<description>>
    #'
    #' @return <<return>>
    #'
    getClassSummary = function() {
      if (is.null(private$class.index) | is.null(private$corpus)) {
        stop("[", class(self)[1], "][FATAL] Dataset was not loaded. Aborting...")
      } else data.frame("N. Instances" = as.matrix(table(private$corpus[, private$class.index])))
    },
    #'
    #' @description get the columns removed or ignored.
    #'
    #' @return a \link{list} containing the name of the removed columns.
    #'
    getRemovedColumns = function() { private$removed.columns },
    #'
    #' @description <<description>>
    #'
    #' @param positive.class <<description>>
    #'
    setPositiveClass = function(positive.class) {
      if (positive.class %in% private$class.values) {
        private$positive.class <- positive.class
      } else { message("[", class(self)[1], "][ERROR] Positive class value not found. ",
                     "Task not performed") }
    },
    #'
    #' @description <<description>>
    #'
    #' @param class.index <<description>>
    #' @param positive.class <<description>>
    #'
    setClassIndex = function(class.index, positive.class) {
      if (class.index %in% 1:ncol(private$corpus)) {
        if ((positive.class %in% as.character(unique(private$corpus[, class.index])))) {
          private$class.values <- as.character(unique(private$corpus[, class.index]))
          private$class.index <- class.index
          private$class.name  <- names(private$corpus)[class.index]
          private$positive.class <- positive.class
        } else { message("[", class(self)[1], "][ERROR] Positive class value not found. ",
                       "Task not performed")}
      } else {
        message("[", class(self)[1], "][ERROR] Class index exceeds dataset limits. ",
                "Must be between 1 and ", ncol(private$corpus), ". Task not performed")
      }
    },
    #'
    #' @description <<description>>
    #'
    #' @param class.name <<description>>
    #' @param positive.class <<description>>
    #'
    setClassName = function(class.name, positive.class) {
      if ((length(which(self$getColumnNames() == class.name)) == 0)) {
        message("[", class(self)[1], "][ERROR] Class name not found. ",
                "Task not performed") }
      else { self$setClassIndex(which(names(private$corpus) == class.name), positive.class) }
    },
    #'
    #' @description removes \link{data.frame} columns matchimg some criterion.
    #'
    #' @param remove.funcs a vector of functions use to define which columns must be removed.
    #' @param remove.na a logical value indicating whether \link{NA} values should be removed.
    #' @param remove.const a logical value used to indicate if constant values should be removed.
    #'
    cleanData = function(remove.funcs = NULL, remove.na = TRUE,
                         remove.const = FALSE) {
      if (isTRUE(remove.na) || isTRUE(remove.const) ||
          (!is.null(remove.funcs) && length(remove.funcs) > 0)) {
        subset <- private$corpus[, -private$class.index]

        if ((!is.null(remove.funcs) && length(remove.funcs) > 0)) {
          for (func in remove.funcs) {
            subset.names <- names(subset)
            subset <- Filter(func(col), subset)
            subset.removed <- setdiff(subset.names, names(subset))
            if (length(subset.removed) > 0) {
              private$removed.columns[["remove.funcs"]] <- append(private$removed.columns[["remove.funcs"]],
                                                                  subset.removed)
            }
          }
          message("[", class(self)[1], "][INFO] Total ",
                  length(private$removed.columns[["remove.funcs"]]),
                  " columns were succesfully removed")
        }


        if (isTRUE(remove.na)) {
          subset.names <- names(subset)
          subset <- Filter(function(col) !all(is.na(col)), subset)
          subset.removed <- setdiff(subset.names, names(subset))
          if (length(subset.removed) > 0) {
            private$removed.columns[["remove.na"]] <- append(private$removed.columns[["na.remove"]],
                                                             subset.removed)
          }
          message("[", class(self)[1], "][INFO] Total ", length(subset.removed),
                  " NA columns were succesfully removed")
        }

        if (isTRUE(remove.const)) {
          subset.names <- names(subset)
          subset <- Filter(function(col) length(unique(col)) > 1, subset)
          subset.removed <- setdiff(subset.names, names(subset))
          if (length(subset.removed) > 0) {
            private$removed.columns[["remove.const"]] <- append(private$removed.columns[["remove.const"]],
                                                                subset.removed)
          }
          message("[", class(self)[1], "][INFO] Total ", length(subset.removed),
                  " const columns were succesfully removed")
        }

        if (private$class.index >= ncol(subset)) {
          subset <- cbind(subset, private$corpus[, private$class.index])
          private$class.index <- ncol(subset)

        } else {
          if (private$class.index == 1) {
            subset <- cbind(subset[, private$class.index], subset)
          } else {
            subset <- cbind(subset[1:private$class.index - 1],
                             private$corpus[, private$class.index],
                             subset[private$class.index:ncol(subset)])
          }
        }
        names(subset)[private$class.index] <- private$class.name
        private$corpus <- subset
      }
    },
    #'
    #' @description applies \code{cleanData} function over an specific set of columns.
    #'
    #' @param columns set of columns (numeric or character) where removal operation should be applied.
    #' @param remove.funcs a vector of functions use to define which columns must be removed.
    #' @param remove.na a logical value indicating whether \code{\link{NA}} values should be removed.
    #' @param remove.const a logical value used to indicate if constant values should be removed.
    #'
    #' @importFrom dplyr between
    #'
    removeColumns = function(columns, remove.funcs = NULL, remove.na = FALSE,
                             remove.const = FALSE) {
      if (is.character(columns)) {
        if (any(columns %in% names(private$corpus))) {
          if (private$class.name %in% columns) {
            columns <- setdiff(columns, private$class.name)
            message("[", class(self)[1], "][INFO] Target class was defined for removal. ",
                    "Ignoring...")
          }
          valid.columns  <- intersect(names(private$corpus), columns)
          private$corpus <- private$corpus[, -which(names(private$corpus) %in% valid.columns)]
          private$class.index <- which(names(private$corpus) == private$class.name)
          private$removed.columns[["manually"]] <- append(private$removed.columns[["manually"]], valid.columns)
          message("[", class(self)[1], "][INFO] Total ", length(valid.columns),
                  " columns were succesfully removed")
        } else {
          message("[", class(self)[1], "][ERROR] Defined column(s) are not valid.",
                  " Task not performed")
        }
      } else {
        if (is.numeric(columns) && !private$class.index %in% columns  &&
            all(dplyr::between(columns, 1, ncol(private$corpus)))) {
          private$removed.columns <- c(private$removed.columns,
                                        names(private$corpus)[columns])
          private$corpus <- private$corpus[, -columns]
          private$class.index <- which(names(private$corpus) == private$class.name)
          message("[", class(self)[1], "][INFO] Total ", length(columns),
                  " columns were succesfully removed")
        } else {
          message("[", class(self)[1], "][ERROR] Selected columns are not valid. ",
                  "Must be between [1-", ncol(private$corpus), "]. ",
                  "Task not performed")
        }
      }
      self$cleanData(remove.funcs, remove.na, remove.const)
      message("[", class(self)[1], "][INFO] Remaining ",
              ncol(private$corpus), " columns")
    },
    #'
    #' @description creates a k-folds partition from the initial dataset.
    #'
    #' @param num.folds an integer for the number of folds (partitions)
    #' @param percent.folds an integer vector with the percentaje of instances containing each fold.
    #' @param class.balance a logical value indicating if class balance should be kept.
    #'
    #' @importFrom caret createFolds
    #'
    createPartitions = function(num.folds = NULL, percent.folds = NULL,
                                class.balance = TRUE) {
      if (((!is.numeric(num.folds) || length(num.folds) != 1) &&
           !is.numeric(percent.folds))) {
        message("[", class(self)[1], "][WARNING] Parameters are invalid. ",
                "Assuming division with default k=10 folds")
        private$partitions <- caret::createFolds(private$corpus[, private$class.index],
                                                  k = 10, list = TRUE)
      } else {
        if (is.numeric(num.folds) && length(num.folds) == 1 && !is.numeric(percent.folds)) {
          message("[", class(self)[1], "][INFO] Perfoming dataset partitioning into ",
                   num.folds, " groups using class balance")
          private$partitions <- caret::createFolds(private$corpus[, private$class.index],
                                                    k = num.folds, list = TRUE)
        } else {
          if (!is.logical(class.balance)) {
            stop("[", class(self)[1], "][FATAL] class.balance not defined. ",
                 "Aborting...")
          }
          if (is.numeric(num.folds) && length(num.folds) == 1 && is.numeric(percent.folds)) {
            if (length(percent.folds) == num.folds &&
                (sum(percent.folds) == 100 || sum(percent.folds) == 1)) {
              if (sum(percent.folds) == 100) {
                percent.folds <- percent.folds / 100
              }
              message("[", class(self)[1], "][INFO] Perfoming dataset ",
                      "partitioning into ", length(percent.folds), " groups")
              remaining <- private$corpus

              numElemFolds <- round(percent.folds * nrow(private$corpus))
              class.percents <- table(private$corpus[, self$getClassIndex()]) / nrow(private$corpus)
              for (index in 1:(num.folds - 1)) {
                message("===============================================================")
                message("[", class(self)[1], "][INFO] Spliting ", index,
                        " group with ", percent.folds[index])
                message("===============================================================")
                if (isTRUE(class.balance)) {
                  if (length(self$getClassValues()) != 2) {
                    stop("[", class(self)[1], "][ERROR] Create partitions with ",
                         "option of class.balance for multiclass data is not ",
                         "still available. Aborting...")
                  }
                  split1 <- sample(which(remaining[, self$getClassIndex()] == names(class.percents)[1]),
                                   round(numElemFolds[[index]] * class.percents[[1]]),
                                   replace = FALSE)
                  split2 <- sample(which(remaining[, self$getClassIndex()] == names(class.percents)[2]),
                                   round(numElemFolds[[index]] * class.percents[[2]]),
                                   replace = FALSE)
                  split <- c(split1, split2)
                } else {
                  split <- sample(1:nrow(remaining), numElemFolds[[index]], replace = FALSE)
                }
                private$partitions <- append(private$partitions,
                                             list(which(rownames(private$corpus) %in% rownames(remaining)[split])))
                remaining <- remaining[-split, ]
              }
              message("===============================================================")
              message("[", class(self)[1], "][INFO] Spliting ", index + 1,
                      " group with ", percent.folds[index + 1])
              message("===============================================================")
              private$partitions <-  append(private$partitions,
                                            list(which(rownames(private$corpus) %in% rownames(remaining))))
              names(private$partitions) <- paste0("Fold0",
                                                  which(1:num.folds < 10))
              if ((num.folds >= 10)) {
                names(private$partitions)[10:num.folds] <- paste0("Fold",
                                                                  which(1:num.folds >= 10))
              }
            } else { message("[", class(self)[1], "][ERROR] Fold partition and/or ",
                             "probability mismatch. Task not performed") }
          } else {
            if ((!is.numeric(num.folds) || length(num.folds) != 1) &&
                is.numeric(percent.folds) && (sum(percent.folds) == 100 ||
                                              sum(percent.folds) == 1)) {
              if (sum(percent.folds) == 100) {
                percent.folds <- percent.folds / 100
              }
              message("[", class(self)[1], "][INFO] Perfoming dataset partitioning into ",
                      length(percent.folds), " groups")
              remaining <- private$corpus

              numElemFolds <- round(percent.folds * nrow(private$corpus))
              class.percents <- table(private$corpus[, self$getClassIndex()]) / nrow(private$corpus)
              for (index in 1:(length(percent.folds) - 1)) {
                message("===============================================================")
                message("[", class(self)[1], "][INFO] Spliting ", index,
                        " group with ", percent.folds[index])
                message("===============================================================")
                if (isTRUE(class.balance)) {
                  if (length(self$getClassValues()) != 2) {
                    stop("[", class(self)[1], "][ERROR] Create partitions with ",
                         "option of class.balance for multiclass data is not ",
                         "still available. Aborting...")
                  }
                  split1 <- sample(which(remaining[, self$getClassIndex()] == names(class.percents)[1]),
                                   round(numElemFolds[[index]] * class.percents[[1]]),
                                   replace = FALSE)
                  split2 <- sample(which(remaining[, self$getClassIndex()] == names(class.percents)[2]),
                                   round(numElemFolds[[index]] * class.percents[[2]]),
                                   replace = FALSE)
                  split <- c(split1, split2)
                } else {
                  split <- sample(1:nrow(remaining), numElemFolds[[index]], replace = FALSE)
                }
                private$partitions <- append(private$partitions,
                                             list(which(rownames(private$corpus) %in% rownames(remaining)[split])))
                remaining <- remaining[-split, ]
              }
              message("===============================================================")
              message("[", class(self)[1], "][INFO] Spliting ", index + 1,
                      " group with ", percent.folds[index + 1])
              message("===============================================================")
              private$partitions <-  append(private$partitions,
                                            list(which(rownames(private$corpus) %in% rownames(remaining))))
              names(private$partitions) <- paste0("Fold0",
                                                  which(1:length(percent.folds) < 10))
              if ((length(percent.folds) >= 10)) {
                names(private$partitions)[10:length(percent.folds)] <- paste0("Fold",
                                                                              which(1:length(percent.folds) >= 10))
              }
            } else {
              message("[", class(self)[1], "][ERROR] Cannot perform ",
                      "partition process. Task not performed")
            }
          }
        }
      }
    },
    #'
    #' @description create a \link{Subset} for testing or classification purposes. A target class should be provided for
    #' testing purposes.
    #'
    #' @param num.folds an integer defining the number of folds that should we used to build the \link{Subset}.
    #' @param column.id an integer or character indicating the column (number or name respectively) identifier. Default \link{NULL} value
    #' is valid ignores defining a identification column.
    #' @param opts a list with optional parameters. Valid arguments are remove.na (removes columns with \link{NA} values) and
    #' remove.const (ignore columns with constant values).
    #'
    #' @return \link{Subset}
    #'
    #' @importFrom dplyr between
    #'
    createSubset = function(num.folds, column.id = NULL,
                            opts = list(remove.na = TRUE, remove.const = FALSE)) {
      subset <- NULL
      if (is.null(private$partitions)) {
        message("[", class(self)[1], "][ERROR] Dataset distribution is null. ",
                "Task not performed")
        return(NULL)
      }
      if (is.null(num.folds) || !is.numeric(num.folds) ||
           !(max(num.folds) %in% c(1:length(private$partitions))))
      {
        message("[", class(self)[1], "][WARNING] Incorrect number of folds. ",
                "Must be between 1 and ", length(private$partitions),
                ". Assuming whole dataset")
        num.folds <- length(private$partitions)
      }

      if (!is.null(column.id) && !(column.id %in% 1:ncol(private$corpus))) {
        message("[", class(self)[1], "][WARNING] Feature identifier is not correct. ",
                "Ignoring value")
        column.id <- NULL
      }

      subset <- private$corpus[ sort(Reduce(union, private$partitions[num.folds])), ]
      class.index <- private$class.index

      if (is.list(opts) && (exists("remove.na", opts) && isTRUE(opts$remove.na) ||
                            exists("remove.const", opts) && isTRUE(opts$remove.const))) {
        na.remov <- 0
        const.remov <- 0
        filtered <- subset[, -private$class.index]

        if (exists("remove.na", opts) && isTRUE(opts$remove.na)) {
          filtered <- Filter(function(col) !all(is.na(col)), filtered)
          na.remov <- ((ncol(subset) - 1) - ncol(filtered))
          message("[", class(self)[1], "][INFO] Removed columns containing NA ",
                  "values (total of ", na.remov, ")")
        }
        if (exists("remove.const", opts) && isTRUE(opts$remove.const)) {
          filtered <- Filter(function(col) all(length(unique(col)) != 1), filtered)
          const.remov <- ((ncol(subset) - 1) - ncol(filtered))
          message("[", class(self)[1], "][INFO] Removed columns containing ",
                  "constant values (total of ", const.remov, ")")
        }

        if (private$class.index >= ncol(filtered)) {
          subset <- cbind(filtered, subset[, private$class.index])
          class.index <- ncol(filtered) + 1
        } else {
          if (private$class.index == 1) {
            subset <- cbind(subset[, private$class.index], filtered)
          } else {
            subset <- cbind(filtered[1:private$class.index - 1],
                             subset[, private$class.index],
                             filtered[private$class.index:ncol(filtered)])
          }
        }
        names(subset)[class.index] <- private$class.name
      }

      Subset$new(dataset = subset, class.index = class.index,
                  class.values = self$getClassValues(),
                  positive.class = self$getPositiveClass())
    },
    #'
    #' @description creates a set for training purposes. A class should be defined to guarantee full-compatibility
    #' with supervised models.
    #'
    #' @param num.folds an integer defining the number of folds that should we used to build the \link{Subset}.
    #' @param opts a list with optional parameters. Valid arguments are remove.na (removes columns with \link{NA} values) and
    #' remove.const (ignore columns with constant values).
    #'
    #' @return \link{Trainset}
    #'
    createTrain = function(num.folds = NULL,
                           opts = list(remove.na = TRUE, remove.const = FALSE)) {
      trainSet <- NULL
      if (is.null(private$partitions)) {
        message("[", class(self)[1], "][ERROR] Dataset distribution is null. ",
                "Task not performed")
        return(NULL)
      }
      if (is.null(num.folds) || !is.numeric(num.folds) ||
           !(max(num.folds) %in% c(1:length(private$partitions))))
      {
        message("[", class(self)[1], "][WARNING] Incorrect number of folds. ",
                "Must be between 1 and ", length(private$partitions),
                ". Assuming whole dataset")
        num.folds <- length(private$partitions)
      }

      trainSet <- private$corpus[ sort(Reduce(union, private$partitions[num.folds])), ]
      class.index <- private$class.index
      if (is.list(opts)) {
        na.remov <- 0
        const.remov <- 0
        filtered <- trainSet[, -private$class.index]

        if (exists("remove.na", opts) && isTRUE(opts$remove.na)) {
          filtered <- Filter(function(col) !all(is.na(col)), filtered)
          na.remov <- ((ncol(trainSet) - 1) - ncol(filtered))
          message("[", class(self)[1], "][INFO] Removed columns containing NA ",
                  "values (total of ", na.remov, ")")
        }
        if (exists("remove.const", opts) && isTRUE(opts$remove.const)) {
          filtered <- Filter(function(col) all(length(unique(col)) != 1), filtered)
          const.remov <- ((ncol(trainSet) - 1) - ncol(filtered))
          message("[", class(self)[1], "][INFO] Removed columns containing ",
                  "constant values (total of ", const.remov, ")")
        }

        if (private$class.index >= ncol(filtered)) {
          trainSet <- cbind(filtered, trainSet[, private$class.index])
          class.index <- ncol(filtered) + 1
        } else {
          if (private$class.index == 1) {
            trainSet <- cbind(trainSet[, private$class.index], filtered)
          } else {
            trainSet <- cbind(filtered[1:private$class.index - 1],
                               trainSet[, private$class.index],
                               filtered[private$class.index:ncol(filtered)])
          }
        }
        names(trainSet)[class.index] <- private$class.name
      }

      Trainset$new(cluster.dist = list(trainSet), class.name = self$getClassName(),
                    class.values = trainSet[, class.index],
                    positive.class = self$getPositiveClass())
    }
  ),
  private = list(
    positive.class = NULL,
    removed.columns = NULL,
    corpus = NULL,
    class.index = NULL,
    class.name = NULL,
    class.values = NULL,
    partitions = NULL
  )
)
