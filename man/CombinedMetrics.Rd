% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prediction.CombinedMetrics.R
\name{CombinedMetrics}
\alias{CombinedMetrics}
\title{Abstract class to compute the class prediction based on combination
between metrics.}
\description{
Abstract class used as a template to define new customized
strategies to combine the class predictions made by different metrics.
}
\seealso{
\code{\link{CombinedVoting}}
}
\keyword{math}
\keyword{methods}
\keyword{models}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-CombinedMetrics-new}{\code{CombinedMetrics$new()}}
\item \href{#method-CombinedMetrics-getRequiredMetrics}{\code{CombinedMetrics$getRequiredMetrics()}}
\item \href{#method-CombinedMetrics-getFinalPrediction}{\code{CombinedMetrics$getFinalPrediction()}}
\item \href{#method-CombinedMetrics-clone}{\code{CombinedMetrics$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-CombinedMetrics-new"></a>}}
\if{latex}{\out{\hypertarget{method-CombinedMetrics-new}{}}}
\subsection{Method \code{new()}}{
Method for initializing the object arguments during runtime.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{CombinedMetrics$new(required.metrics)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{required.metrics}}{A \link{character} vector of length greater than
2 with the name of the required metrics.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-CombinedMetrics-getRequiredMetrics"></a>}}
\if{latex}{\out{\hypertarget{method-CombinedMetrics-getRequiredMetrics}{}}}
\subsection{Method \code{getRequiredMetrics()}}{
The function returns the required metrics that will
participate in the combined metric process.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{CombinedMetrics$getRequiredMetrics()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
A \link{character} vector of length greater than 2 with the name
of the required metrics.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-CombinedMetrics-getFinalPrediction"></a>}}
\if{latex}{\out{\hypertarget{method-CombinedMetrics-getFinalPrediction}{}}}
\subsection{Method \code{getFinalPrediction()}}{
Function used to implement the strategy to obtain the final
prediction based on different metrics.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{CombinedMetrics$getFinalPrediction(
  raw.pred,
  prob.pred,
  positive.class,
  negative.class
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{raw.pred}}{A \link{character} list of length greater than 2 with the
class value of the predictions made by the metrics.}

\item{\code{prob.pred}}{A \link{numeric} list of length greater than 2 with the
probability of the predictions made by the metrics.}

\item{\code{positive.class}}{A \link{character} with the value of the positive
class.}

\item{\code{negative.class}}{A \link{character} with the value of the negative
class.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A \link{logical} value indicating if the instance is predicted as
positive class or not.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-CombinedMetrics-clone"></a>}}
\if{latex}{\out{\hypertarget{method-CombinedMetrics-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{CombinedMetrics$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
