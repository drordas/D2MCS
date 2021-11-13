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

#'
#' @title Write messages to the log at a given priority level using
#' the custom d2mcs log
#'
#' @description \code{d2mcs.log} is responsible for managing the messages to
#' show on the log.
#'
#' @param message A string to be printed to the log with the corresponding
#' priority level.
#' @param level The desired priority level (DEBUG,INFO,WARN,ERROR and FATAL).
#' In the case of the FATAL level will be call to the stop function. Also, if
#' the level is WARN, the message will be a warning.
#' @param className A string to indicated in which class is called to the log.
#' If the value is NULL, this field is not shown in the log.
#' @param methodName A string to indicated in which method is called to the log.
#' If the value is NULL, this field is not shown in the log.
#'
#' @section Details:
#' The format output is as following:
#'
#' [currentTime][className][methodName][level] message
#'
#' The type of message changes according to the level indicated:
#'
#' - The \strong{DEBUG},\strong{INFO} and \strong{ERROR} levels return a text
#' using the \code{\link{message}} function.
#'
#' - The \strong{WARN} level returns a text using the \code{\link{warning}} function.
#'
#' - The \strong{FATAL} level returns a text using the \code{\link{stop}} function.
#'
#' @note In the case of multithreading, the log will only be by file.
#'
#' @examples
#' \dontrun{
#'
#' # First step, configure the behavior of log
#'
#' d2mcs.options$configureLog(console = TRUE, threshold = "DEBUG", file = NULL)
#'
#' message <- "Message example"
#'
#' className <- "Class name example"
#'
#' methodName <- "Method name example"
#'
#' d2mcs.log(message = message, level = "DEBUG", className = NULL, methodName = NULL)
#'
#' d2mcs.log(message = message, level = "INFO", className = className, methodName = methodName)
#'
#' d2mcs.log(message = message, level = "WARN", className = className, methodName = NULL)
#'
#' d2mcs.log(message = message, level = "ERROR", className = NULL, methodName = NULL)
#'
#' d2mcs.log(message = message, level = "FATAL", className = NULL, methodName = methodName)
#' }
#'
#' @keywords NULL
#'
#' @export d2mcs.log
#' @seealso \code{\link{d2mcs.Options}}
#' @include wrapper.R

d2mcs.log <- function(message, level = "INFO", className = NULL, methodName = NULL) {

  if (!d2mcs.Options$isSpecificOption("verbose") ||
      is.null(d2mcs.Options$get("verbose"))) {
    stop("[", format(Sys.time()), "]","[d2mcs.log][FATAL] Verbose is not defined",
         " in d2mcs.Options")
  }

  if (is.null(level) ||
      !is.character(level) ||
      !any(c("FATAL", "ERROR", "WARN", "INFO", "DEBUG") %in% level)) {
    stop("[", format(Sys.time()), "]","[d2mcs.log][FATAL] The 'level' variable ",
         "must be between these values: FATAL, ERROR, WARN, INFO or DEBUG")
  }

  if (d2mcs.Options$get("verbose") || level %in% c("FATAL", "WARN")) {

    settings <- .getLoggerSettings()

    if (is.null(settings$loggers)) {
      stop("[", format(Sys.time()), "]","[d2mcs.log][FATAL] Logger is not ",
           "configured. Use d2mcs.options$configureLog to configure its behavior")
    }

    if (length(settings$loggers) == 0) {
      if (level == "FATAL") {
        if (!is.null(className)) {
          className <- paste0("[", className, "]")
        }
        if (!is.null(methodName)) {
          methodName <- paste0("[", methodName, "]")
        }
        stop("[", format(Sys.time()), "]", className, methodName,
             "[", level, "] ", message, call. = FALSE)
      } else {
        if (level == "WARN") {
          if (!is.null(className)) {
            className <- paste0("[", className, "]")
          }
          if (!is.null(methodName)) {
            methodName <- paste0("[", methodName, "]")
          }
          warning("[", format(Sys.time()), "]", className, methodName,
                  "[", level, "] ", message, call. = FALSE)
        }
      }
    } else {
      for (logger in settings$loggers) {
        if (.levelToInt(level) >= .levelToInt(logger$threshold)) {
          logger$.logFunction(this = logger, level = level, message = list(className, methodName, message))
        }
      }
    }
  }
}
