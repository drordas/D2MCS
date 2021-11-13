#
# D2MCS provides a novel framework to able to automatically develop and deploy
# an accurate Multiple Classifier System (MCS) based on the feature-clustering
# distribution achieved from an input dataset. D2MCS was developed focused on
# four main aspects: (i) the ability to determine an effective method to
# evaluate the independence of features, (ii) the identification of the optimal
# number of feature clusters, (iii) the training and tuning of ML models and
# (iv) the execution of voting schemes to combine the outputs of each classifier
# comprising the MCS.
#
# Copyright (C) 2021 Sing Group (University of Vigo)
#
# This program is free software: you can redistribute it and/or modify it under
# the terms of the GNU General Public License as published by the Free Software
# Foundation, either version 3 of the License, or (at your option) any later
# version.
#
# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License along with
# this program. If not, see <https://www.gnu.org/licenses/gpl-3.0.html>

#' @import R6
#' @include wrapper.R

D2MCSOptions <- R6::R6Class(

  "D2MCSOptions",

  public = list(

    initialize = function() {

      private$d2mcs.options <- list(verbose = TRUE)

      private$d2mcs.log.layout.console <- function(level, ...) {

        dots <- list(...)
        className <- ""
        methodName <- ""

        if (!is.null(dots[[1]][[1]])) {
          className <- paste0("[", dots[[1]][[1]], "]")
        }
        if (!is.null(dots[[1]][[2]])) {
          methodName <- paste0("[", dots[[1]][[2]], "]")
        }

        message <- dots[[1]][[3]]

        if (identical(level, "DEBUG") ||
            identical(level, "INFO") ||
            identical(level, "ERROR")) {
          message("[", format(Sys.time()), "]", className, methodName,
                  "[", level, "] ", message)
        } else {
          if (identical(level, "WARN")) {
            warning("[", format(Sys.time()), "]", className, methodName,
                    "[", level, "] ", message, call. = FALSE)
          } else {
            if (identical(level, "FATAL")) {
              stop("[", format(Sys.time()), "]", className, methodName,
                   "[", level, "] ", message, call. = FALSE)
            }
          }
        }
      }

      private$d2mcs.log.layout.file <- function(level, ...) {

        dots <- list(...)
        className <- ""
        methodName <- ""

        if (!is.null(dots[[1]][[1]])) {
          className <- paste0("[", dots[[1]][[1]], "]")
        }
        if (!is.null(dots[[1]][[2]])) {
          methodName <- paste0("[", dots[[1]][[2]], "]")
        }
        message <- dots[[1]][[3]]

        paste0("[", format(Sys.time()), "]", className, methodName,
               "[", level, "] ", message, collapse = "")
      }

      private$threshold <- "INFO"
      appenders <- list(private$console_appender_d2mcs(layout = private$d2mcs.log.layout.console))

      .clearLoggers()
      logger <- private$createLoggerCustom(name = "SIMPLE",
                                           threshold = private$threshold,
                                           appenders = appenders)
      .registerLogger(logger)
      private$d2mcs.log.console <-  TRUE
    },

    get = function(key) {

      if (!"character" %in% class(key)) {
        stop("[", format(Sys.time()), "][", class(self)[1], "]",
             "[get][FATAL] Checking the type of the 'key' variable: ",
             class(key))
      }

      if (!key %in% names(private$d2mcs.options)) {
        stop("[", format(Sys.time()), "][", class(self)[1], "]",
             "[get][FATAL] '", key, "' option is not configured")
      }
      private$d2mcs.options[[key]]
    },

    add = function(key, value) {

      if (!"character" %in% class(key)) {
        stop("[", format(Sys.time()), "][", class(self)[1], "]",
             "[add][FATAL] Checking the type of the 'key' variable: ",
             class(key))
      }

      if (self$isSpecificOption(key)) {
        stop("[", format(Sys.time()), "][", class(self)[1], "]",
             "[add][FATAL] '", key, "' option is already ",
             "configured with the value: ", self$get(key))
      } else {
        private$d2mcs.options <- append(private$d2mcs.options, value)
        names(private$d2mcs.options)[length(private$d2mcs.options)] <- key
      }
    },

    set = function(key, value) {

      if (!"character" %in% class(key)) {
        stop("[", format(Sys.time()), "][", class(self)[1], "]",
             "[set][FATAL] Checking the type of the 'key' variable: ",
             class(key))
      }

      if (!self$isSpecificOption(key)) {
        stop("[", format(Sys.time()), "][", class(self)[1], "]",
             "[set][FATAL] '", key, "' option is not configured")
      } else {
        if (is.null(value)) {
          private$d2mcs.options[key] <- list(value)
        } else {
          private$d2mcs.options[[key]] <- value
        }
      }
    },

    remove = function(key) {
      if (!"character" %in% class(key)) {
        stop("[", format(Sys.time()), "][", class(self)[1], "]",
             "[remove][FATAL] Checking the type of the 'key' variable: ",
             class(key))
      }

      if (!self$isSpecificOption(key)) {
        stop("[", format(Sys.time()), "][", class(self)[1], "]",
             "[remove][FATAL] '", key, "' option is not configured")
      } else {
        private$d2mcs.options <- private$d2mcs.options[names(private$d2mcs.options) != key]
      }
    },

    getAll = function() {
      private$d2mcs.options
    },

    reset = function() {
      private$d2mcs.options <- list(verbose = TRUE)
    },

    isSpecificOption = function(key) {
      key %in% names(private$d2mcs.options)
    },

    configureLog = function(console = TRUE, threshold = "INFO", file = NULL) {

      if (is.null(threshold) ||
          !is.character(threshold) ||
          !any(c("FATAL", "ERROR", "WARN", "INFO", "DEBUG") %in% threshold)) {
        stop("[", format(Sys.time()), "][", class(self)[1], "]",
             "[configureLog][FATAL] The 'threshold' parameter ",
             "must be between these values: FATAL, ERROR, WARN, INFO or DEBUG")
      }

      if (!"logical" %in% class(console)) {
        stop("[", format(Sys.time()), "][", class(self)[1], "]",
             "[configureLog][FATAL] ",
             "Checking the type of the 'console' variable: ",
             class(console))
      }

      private$threshold <- threshold
      .clearLoggers()

      if (!is.null(file) &&
          is.character(file)) {
        appenders <- list(private$file_appender_d2mcs(layout  = private$d2mcs.log.layout.file,
                                                      fileName = file))

        logger <- private$createLoggerCustom(name = "file_logger",
                                             threshold = threshold,
                                             appenders = appenders)
        .registerLogger(logger)

        private$d2mcs.log.file <- file
      } else {
        private$d2mcs.log.file <- FALSE
      }

      if (console) {
        appenders <- list(private$console_appender_d2mcs(layout = private$d2mcs.log.layout.console))

        logger <- private$createLoggerCustom(name = "console_logger",
                                             threshold = threshold,
                                             appenders = appenders)
        .registerLogger(logger)

        private$d2mcs.log.console <- TRUE
      } else {
        private$d2mcs.log.console <- FALSE
      }

      if (all(isFALSE(private$d2mcs.log.file),
              isFALSE(private$d2mcs.log.console))) {
        .clearLoggers()
      }
    },

    disableLog = function() {
      .clearLoggers()
      private$d2mcs.log.console <- FALSE
      private$d2mcs.log.file <- FALSE
      private$threshold <- "INFO"
    },

    getLogConfiguration = function() {
      message("[", format(Sys.time()), "][", class(self)[1], "]",
              "[getLogConfiguration][INFO] Log configuration:\n",
              "\t- Threshold: ",
              ifelse(all(!private$d2mcs.log.console,
                         is.null(private$d2mcs.log.file)),
                     "Not configured",
                     as.character(private$threshold)), "\n",
              "\t- Console log status: ",
              ifelse(private$d2mcs.log.console,
                     "Actived",
                     "Disabled"), "\n",
              "\t- File log status: ",
              ifelse(!isFALSE(private$d2mcs.log.file),
                     paste0("Actived. File asociated: ", private$d2mcs.log.file),
                     "Disabled"))
    },

    print = function(...) {
      print(self$getAll())
    }
  ),

  private = list(
    d2mcs.options = list(),
    threshold = "",
    d2mcs.log.layout.console = NULL,
    d2mcs.log.layout.file = NULL,
    d2mcs.log.console = FALSE,
    d2mcs.log.file = FALSE,
    console_appender_d2mcs = function(layout = private$d2mcs.log.layout.console) {
      .appendFunction <- function(this, level, message) { }
      appender <- list(.appendFunction = .appendFunction,
                       layout = layout)
      class(appender) <- "Appender"
      appender
    },

    file_appender_d2mcs = function(layout = private$d2mcs.log.layout.file, fileName) {
      .appendFunction <- function(this, level, message) {
        con <- file(fileName, open = "at", blocking = FALSE)
        writeLines(text = message, con = con)
        flush(con)
        close(con)
      }
      appender <- list(.appendFunction = .appendFunction,
                       layout = layout,
                       fileName = fileName)
      class(appender) <- "Appender"
      appender
    },

    createLoggerCustom = function(name = "SIMPLE",
                                  threshold = "INFO",
                                  appenders) {

      .logFunction <- function(this, level, message) {
        for (appender in this$appenders) {
          formatted <- appender$layout(level, message)
          appender$.appendFunction(appender, level, formatted)
        }
      }

      logger <- list(name = name,
                     .logFunction = .logFunction,
                     threshold = threshold,
                     appenders = appenders)
      class(logger) <- "Logger"
      logger
    }
  )
)
