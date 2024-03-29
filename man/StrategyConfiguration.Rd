% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/clustering.strategies.StrategyConfiguration.R
\name{StrategyConfiguration}
\alias{StrategyConfiguration}
\title{Default Strategy Configuration handler.}
\description{
Define default configuration parameters for the clustering
strategies.
}
\details{
The \code{\link{StrategyConfiguration}} can be used to define the
default configuration parameters for a feature clustering strategy or as an
archetype to define new customized parameters.
}
\seealso{
\code{\link{DependencyBasedStrategyConfiguration}}
}
\keyword{cluster}
\keyword{manip}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-StrategyConfiguration-new}{\code{StrategyConfiguration$new()}}
\item \href{#method-StrategyConfiguration-minNumClusters}{\code{StrategyConfiguration$minNumClusters()}}
\item \href{#method-StrategyConfiguration-maxNumClusters}{\code{StrategyConfiguration$maxNumClusters()}}
\item \href{#method-StrategyConfiguration-clone}{\code{StrategyConfiguration$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-StrategyConfiguration-new"></a>}}
\if{latex}{\out{\hypertarget{method-StrategyConfiguration-new}{}}}
\subsection{Method \code{new()}}{
Empty function used to initialize the object arguments in
runtime.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{StrategyConfiguration$new()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-StrategyConfiguration-minNumClusters"></a>}}
\if{latex}{\out{\hypertarget{method-StrategyConfiguration-minNumClusters}{}}}
\subsection{Method \code{minNumClusters()}}{
Function used to return the minimum number of clusters
distributions used. By default the minimum is set in 2.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{StrategyConfiguration$minNumClusters(...)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{...}}{Further arguments passed down to \code{minNumClusters}
function.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A \link{numeric} vector of length 1.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-StrategyConfiguration-maxNumClusters"></a>}}
\if{latex}{\out{\hypertarget{method-StrategyConfiguration-maxNumClusters}{}}}
\subsection{Method \code{maxNumClusters()}}{
The function is responsible of returning the maximum number
of cluster distributions used. By default the maximum number is set in 50.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{StrategyConfiguration$maxNumClusters(...)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{...}}{Further arguments passed down to \code{maxNumClusters}
function.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A \link{numeric} vector of length 1.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-StrategyConfiguration-clone"></a>}}
\if{latex}{\out{\hypertarget{method-StrategyConfiguration-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{StrategyConfiguration$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
