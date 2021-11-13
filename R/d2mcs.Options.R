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

#' @title Object to handle the keys/attributes/options common on D2MCS
#'
#' @description This class provides the necessary methods to manage a list of
#' keys or options used along the MCS creation, both those provided by the default
#' library and those implemented by the user.
#'
#' @docType class
#'
#' @format NULL
#'
#' @section Details:
#' By default, the application initializes the object named \code{d2mcs.Options}
#' of type \code{D2MCSOptions} which is in charge of initializing the possible
#' options used in the D2MCS.
#'
#' The default fields on \code{\link{d2mcs.Options}} are initialized, if needed,
#' as shown bellow:
#'
#' \strong{[verbose]}
#'
#' - \code{d2mcs.Options$set("verbose", <<status_verbose>>)}
#'
#' @section Log configuration:
#'
#' The d2mcs log is configured through the \code{configureLog} function.
#' This system manages both the place to display the messages and the priority
#' level of each message showing only the messages with a higher level than
#' indicated in the \emph{threshold} variable.
#'
#' If you want to deactivate the d2mcs log, the \code{disableLog}
#' method in \code{\link{d2mcs.Options}} does this task.
#'
#' @section Methods:
#' \itemize{
#' \item{\bold{get:}}{
#' obtains a specific option.
#' \itemize{
#' \item{\emph{Usage:}}{
#' \code{get(key)}
#' }
#' \item{\emph{Value:}}{
#' the value of the specific option.
#' }
#' \item{\emph{Arguments:}}{
#' \itemize{
#' \item{\strong{key:}}{
#' (\emph{character}) the name of the option to obtain.
#' }
#' }
#' }
#' }
#' }
#'
#' \item{\bold{add:}}{
#' adds a option to the list of options
#' \itemize{
#' \item{\emph{Usage:}}{
#' \code{add(key, value)}
#' }
#' \item{\emph{Arguments:}}{
#' \itemize{
#' \item{\strong{key:}}{
#' (\emph{character}) the name of the new option.
#' }
#' \item{\strong{propertyName:}}{
#' (\emph{Object}) the value of the new option.
#' }
#' }
#' }
#' }
#' }
#'
#' \item{\bold{set:}}{
#' modifies the value of the one option.
#' \itemize{
#' \item{\emph{Usage:}}{
#' \code{set(key, value)}
#' }
#' \item{\emph{Arguments:}}{
#' \itemize{
#' \item{\strong{key:}}{
#' (\emph{character}) the name of the new option.
#' }
#' \item{\strong{propertyName:}}{
#' (\emph{Object}) the value of the new option.
#' }
#' }
#' }
#' }
#' }
#'
#' \item{\bold{remove:}}{
#' removes a specific option.
#' \itemize{
#' \item{\emph{Usage:}}{
#' \code{remove(key)}
#' }
#' \item{\emph{Arguments:}}{
#' \itemize{
#' \item{\strong{key:}}{
#' (\emph{character}) the name of the option to remove.
#' }
#' }
#' }
#' }
#' }
#'
#' \item{\bold{getAll:}}{
#' gets the list of options.
#' \itemize{
#' \item{\emph{Usage:}}{
#' \code{getAll()}
#' }
#' \item{\emph{Value:}}{
#' Value of options.
#' }
#' }
#' }
#'
#' \item{\bold{remove:}}{
#' resets the option list to the initial state.
#' \itemize{
#' \item{\emph{Usage:}}{
#' \code{reset()}
#' }
#' }
#' }
#'
#' \item{\bold{isSpecificOption:}}{
#' checks for the existence of an specific option.
#' \itemize{
#' \item{\emph{Usage:}}{
#' \code{isSpecificProperty(key)}
#' }
#' \item{\emph{Value:}}{
#' A boolean results according to the existence of the specific option in the list of options
#' }
#' \item{\emph{Arguments:}}{
#' \itemize{
#' \item{\strong{key:}}{
#' (\emph{character}) the key of the option to check.
#' }
#' }
#' }
#' }
#' }
#'
#' \item{\bold{configureLog:}}{
#' Configures the D2MCS log.
#' \itemize{
#' \item{\emph{Usage:}}{
#' \code{configureLog(console = TRUE, threshold = "INFO", file = NULL)}
#' }
#' \item{\emph{Arguments:}}{
#' \itemize{
#' \item{\strong{console:}}{
#' (\emph{boolean}) Shows the log on console or not.
#' }
#' \item{\strong{threshold:}}{
#' (\emph{character}) The logging threshold level. Messages with a lower
#' priority level will be discarded.
#' }
#' \item{\strong{file:}}{
#' (\emph{character}) The file to write messages to. If it is NULL,
#' the log in file will not be enabled.
#' }
#' }
#' }
#' }
#' }
#'
#' \item{\bold{disableLog:}}{
#' Deactivates the D2MCS log.
#' \itemize{
#' \item{\emph{Usage:}}{
#' \code{disableLog()}
#' }
#' }
#' }
#'
#' \item{\bold{getLogConfiguration:}}{
#' Print the D2MCS log configuration.
#' \itemize{
#' \item{\emph{Usage:}}{
#' \code{getLogConfiguration()}
#' }
#' }
#' }
#' }
#'
#' @seealso \code{\link{d2mcs.log}}
#'
#' @keywords NULL
#'
#' @export d2mcs.Options

d2mcs.Options <- D2MCSOptions$new()
