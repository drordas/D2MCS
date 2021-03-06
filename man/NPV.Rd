% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/measures.NPV.R
\name{NPV}
\alias{NPV}
\title{Computes the Negative Predictive Value.}
\description{
Negative Predictive Values are the proportions of negative
results in statistics and diagnostic tests that are true negative results.
}
\details{
\deqn{NPV = TN / (TN + FN)}
}
\seealso{
\code{\link{MeasureFunction}}, \code{\link{ClassificationOutput}},
\code{\link{ConfMatrix}}
}
\keyword{classif}
\keyword{math}
\section{Super class}{
\code{\link[D2MCS:MeasureFunction]{D2MCS::MeasureFunction}} -> \code{NPV}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-new}{\code{NPV$new()}}
\item \href{#method-compute}{\code{NPV$compute()}}
\item \href{#method-clone}{\code{NPV$clone()}}
}
}
\if{html}{
\out{<details open ><summary>Inherited methods</summary>}
\itemize{
}
\out{</details>}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-new"></a>}}
\if{latex}{\out{\hypertarget{method-new}{}}}
\subsection{Method \code{new()}}{
Method for initializing the object arguments during runtime.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{NPV$new(performance.output = NULL)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{performance.output}}{An optional \code{\link{ConfMatrix}} parameter
to define the type of object used as basis to compute the \code{NPV}
measure.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-compute"></a>}}
\if{latex}{\out{\hypertarget{method-compute}{}}}
\subsection{Method \code{compute()}}{
The function computes the \strong{NPV} achieved by the M.L.
model.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{NPV$compute(performance.output = NULL)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{performance.output}}{An optional \code{\link{ConfMatrix}}  parameter
to define the type of object used as basis to compute the \strong{NPV}
measure.}
}
\if{html}{\out{</div>}}
}
\subsection{Details}{
This function is automatically invoke by the
\link{ClassificationOutput} object.
}

\subsection{Returns}{
A \link{numeric} vector of size 1 or \link{NULL} if an error
occurred.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-clone"></a>}}
\if{latex}{\out{\hypertarget{method-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{NPV$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
