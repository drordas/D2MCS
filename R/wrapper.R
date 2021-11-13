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

# Wrappers to adapt functions of ParallelLogger package to D2MCS package.

#'
#'@include d2mcs.log.R
#'
.clearLoggers <- function() {
  settings <- .getLoggerSettings()
  settings$loggers <- list()
  .setLoggerSettings(settings)
}

.getLoggerSettings <- function() {
  getOption("loggerSettings")
}

.setLoggerSettings <- function(settings) {
  options(loggerSettings = settings)
}

.registerLogger <- function(logger) {
  settings <- .getLoggerSettings()
  settings$loggers[[length(settings$loggers) + 1]] <- logger
  .setLoggerSettings(settings)
  invisible(NULL)
}

.levelToInt <- function(level) {
  if (level == "DEBUG")
    return(2)
  if (level == "INFO")
    return(3)
  if (level == "WARN")
    return(4)
  if (level == "ERROR")
    return(5)
  if (level == "FATAL")
    return(6)
}
