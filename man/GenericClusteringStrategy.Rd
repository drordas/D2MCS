% Generated by roxygen2: do not edit by hand
% Please edit documentation in
%   R/clustering.strategies.GenericClusteringStrategy.R
\name{GenericClusteringStrategy}
\alias{GenericClusteringStrategy}
\title{Abstract Feature Clustering Strategy class.}
\description{
Abstract class used as a template to ensure the proper
definition of new customized clustering strategies.
}
\details{
The \link{GenericClusteringStrategy} is an archetype class so it
cannot be instantiated.
}
\seealso{
\code{\link{Subset}}, \code{\link{GenericHeuristic}}
}
\keyword{cluster}
\keyword{manip}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-new}{\code{GenericClusteringStrategy$new()}}
\item \href{#method-getDescription}{\code{GenericClusteringStrategy$getDescription()}}
\item \href{#method-getHeuristic}{\code{GenericClusteringStrategy$getHeuristic()}}
\item \href{#method-getConfiguration}{\code{GenericClusteringStrategy$getConfiguration()}}
\item \href{#method-getBestClusterDistribution}{\code{GenericClusteringStrategy$getBestClusterDistribution()}}
\item \href{#method-getUnclustered}{\code{GenericClusteringStrategy$getUnclustered()}}
\item \href{#method-execute}{\code{GenericClusteringStrategy$execute()}}
\item \href{#method-getDistribution}{\code{GenericClusteringStrategy$getDistribution()}}
\item \href{#method-createTrain}{\code{GenericClusteringStrategy$createTrain()}}
\item \href{#method-plot}{\code{GenericClusteringStrategy$plot()}}
\item \href{#method-saveCSV}{\code{GenericClusteringStrategy$saveCSV()}}
\item \href{#method-clone}{\code{GenericClusteringStrategy$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-new"></a>}}
\if{latex}{\out{\hypertarget{method-new}{}}}
\subsection{Method \code{new()}}{
A function responsible for creating a
\link{GenericClusteringStrategy} object.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{GenericClusteringStrategy$new(subset, heuristic, description, configuration)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{subset}}{A \code{\link{Subset}} object to perform the clustering strategy.}

\item{\code{heuristic}}{The heuristic to be applied. Must inherit from
\code{\link{GenericHeuristic}} class.}

\item{\code{description}}{A \link{character} vector describing the strategy
operation.}

\item{\code{configuration}}{Optional customized configuration parameters for the
strategy. Must inherited from \code{\link{StrategyConfiguration}}
abstract class.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-getDescription"></a>}}
\if{latex}{\out{\hypertarget{method-getDescription}{}}}
\subsection{Method \code{getDescription()}}{
The function is used to obtain the description of the
strategy.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{GenericClusteringStrategy$getDescription()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
A \link{character} vector of \link{NULL} if not defined.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-getHeuristic"></a>}}
\if{latex}{\out{\hypertarget{method-getHeuristic}{}}}
\subsection{Method \code{getHeuristic()}}{
The function returns the heuristic applied for the
clustering strategy.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{GenericClusteringStrategy$getHeuristic()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
An object inherited from \code{\link{GenericClusteringStrategy}}
class.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-getConfiguration"></a>}}
\if{latex}{\out{\hypertarget{method-getConfiguration}{}}}
\subsection{Method \code{getConfiguration()}}{
The function returns the configuration parameters used to
perform the clustering strategy.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{GenericClusteringStrategy$getConfiguration()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
An object inherited from \code{\link{StrategyConfiguration}}
class.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-getBestClusterDistribution"></a>}}
\if{latex}{\out{\hypertarget{method-getBestClusterDistribution}{}}}
\subsection{Method \code{getBestClusterDistribution()}}{
The function obtains the best clustering distribution.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{GenericClusteringStrategy$getBestClusterDistribution()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
A \link{list} of clusters. Each list element represents a feature
group.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-getUnclustered"></a>}}
\if{latex}{\out{\hypertarget{method-getUnclustered}{}}}
\subsection{Method \code{getUnclustered()}}{
The function is used to return the features that cannot be
clustered due to incompatibilities with the used heuristic.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{GenericClusteringStrategy$getUnclustered()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
A \link{character} vector containing the unclassified features.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-execute"></a>}}
\if{latex}{\out{\hypertarget{method-execute}{}}}
\subsection{Method \code{execute()}}{
Abstract function responsible of performing the clustering
strategy over the defined \code{\link{Subset}}.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{GenericClusteringStrategy$execute(verbose, ...)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{verbose}}{A \link{logical} value to specify if more verbosity is
needed.}

\item{\code{...}}{Further arguments passed down to \code{execute} function.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-getDistribution"></a>}}
\if{latex}{\out{\hypertarget{method-getDistribution}{}}}
\subsection{Method \code{getDistribution()}}{
Abstract function used to obtain the set of features
following an specific clustering distribution.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{GenericClusteringStrategy$getDistribution(
  num.clusters = NULL,
  num.groups = NULL,
  include.unclustered = FALSE
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{num.clusters}}{A \link{numeric} value to select the number of
clusters (define the distribution).}

\item{\code{num.groups}}{A single or \link{numeric} vector value to identify a
specific group that forms the clustering distribution.}

\item{\code{include.unclustered}}{A \link{logical} value to determine if
unclustered features should be included.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A \link{list} with the features comprising an specific clustering
distribution.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-createTrain"></a>}}
\if{latex}{\out{\hypertarget{method-createTrain}{}}}
\subsection{Method \code{createTrain()}}{
Abstract function in charge of creating a
\code{\link{Trainset}} object for training purposes.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{GenericClusteringStrategy$createTrain(
  subset,
  num.cluster = NULL,
  num.groups = NULL,
  include.unclustered = FALSE
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{subset}}{A \code{\link{Subset}} object used as a basis to create the
\link{Trainset}}

\item{\code{num.cluster}}{A \link{numeric} value to select the number of
clusters (define the distribution).}

\item{\code{num.groups}}{A single or \link{numeric} vector value to identify a
specific group that forms the clustering distribution.}

\item{\code{include.unclustered}}{A \link{logical} value to determine if
unclustered features should be included.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-plot"></a>}}
\if{latex}{\out{\hypertarget{method-plot}{}}}
\subsection{Method \code{plot()}}{
Abstract function responsible of creating a plot to
visualize the clustering distribution.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{GenericClusteringStrategy$plot(dir.path = NULL, file.name = NULL, ...)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{dir.path}}{An optional \link{character} argument to define the name
of the directory where the exported plot will be saved. If not defined,
the file path will be automatically assigned to the current working
directory, '\code{getwd()}'.}

\item{\code{file.name}}{The name of the PDF file where the plot is exported.}

\item{\code{...}}{Further arguments passed down to \code{execute} function.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-saveCSV"></a>}}
\if{latex}{\out{\hypertarget{method-saveCSV}{}}}
\subsection{Method \code{saveCSV()}}{
Abstract function to save the clustering distribution to a
CSV file.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{GenericClusteringStrategy$saveCSV(dir.path, name, num.clusters = NULL)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{dir.path}}{The name of the directory to save the CSV file.}

\item{\code{name}}{Defines the name of the CSV file.}

\item{\code{num.clusters}}{An optional parameter to select the number of
clusters to be saved. If not defined, all clusters will be saved.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-clone"></a>}}
\if{latex}{\out{\hypertarget{method-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{GenericClusteringStrategy$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
