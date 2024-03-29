% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.HDSubset.R
\name{HDSubset}
\alias{HDSubset}
\title{High Dimensional Subset handler.}
\description{
Creates a high dimensional subset from a \code{\link{HDDataset}}
object. Only the required instances are loaded in memory to avoid unnecessary
use of resources and memory.
}
\details{
Use \code{\link{HDDataset}} to ensure the creation of a valid
\code{\link{HDSubset}} object.
}
\seealso{
\code{\link{HDDataset}}, \code{\link{DatasetLoader}}
}
\keyword{attribute}
\keyword{datagen}
\keyword{datasets}
\keyword{manip}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-HDSubset-new}{\code{HDSubset$new()}}
\item \href{#method-HDSubset-getColumnNames}{\code{HDSubset$getColumnNames()}}
\item \href{#method-HDSubset-getNcol}{\code{HDSubset$getNcol()}}
\item \href{#method-HDSubset-getID}{\code{HDSubset$getID()}}
\item \href{#method-HDSubset-getIterator}{\code{HDSubset$getIterator()}}
\item \href{#method-HDSubset-isBlinded}{\code{HDSubset$isBlinded()}}
\item \href{#method-HDSubset-clone}{\code{HDSubset$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-HDSubset-new"></a>}}
\if{latex}{\out{\hypertarget{method-HDSubset-new}{}}}
\subsection{Method \code{new()}}{
Method for initializing the object arguments during runtime.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{HDSubset$new(
  file.path,
  feature.names,
  feature.id,
  start.at = 0,
  sep = ",",
  chunk.size
)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{file.path}}{The name of the file which the data are to be read from.
Each row of the table appears as one line of the file. If it does not
contain an _absolute_ path, the file name is _relative_ to the current
working directory, '\code{getwd()}'.}

\item{\code{feature.names}}{A \link{character} vector specifying the name of the
features that should be included in the \code{\link{HDDataset}} object.}

\item{\code{feature.id}}{An \link{integer} or \link{character} indicating the
column (number or name respectively) identifier. Default \link{NULL}
value is valid ignores defining a identification column.}

\item{\code{start.at}}{A \link{numeric} value to identify the reading start
position.}

\item{\code{sep}}{the field separator character. Values on each line of the file
are separated by this character.}

\item{\code{chunk.size}}{an \link{integer} value indicating the size of chunks
taken over each iteration. By default chunk.size is defined as 10000.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-HDSubset-getColumnNames"></a>}}
\if{latex}{\out{\hypertarget{method-HDSubset-getColumnNames}{}}}
\subsection{Method \code{getColumnNames()}}{
Gets the name of the columns comprising the subset.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{HDSubset$getColumnNames()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
A \link{character} vector containing the name of each column.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-HDSubset-getNcol"></a>}}
\if{latex}{\out{\hypertarget{method-HDSubset-getNcol}{}}}
\subsection{Method \code{getNcol()}}{
Obtains the number of columns present in the dataset.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{HDSubset$getNcol()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
A \link{numeric} value or 0 if is empty.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-HDSubset-getID"></a>}}
\if{latex}{\out{\hypertarget{method-HDSubset-getID}{}}}
\subsection{Method \code{getID()}}{
Obtains the column identifier.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{HDSubset$getID()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
A \link{character} vector of size 1.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-HDSubset-getIterator"></a>}}
\if{latex}{\out{\hypertarget{method-HDSubset-getIterator}{}}}
\subsection{Method \code{getIterator()}}{
Creates the \code{\link{FIterator}} object.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{HDSubset$getIterator(chunk.size = private$chunk.size, verbose = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{chunk.size}}{An \link{integer} value indicating the size of chunks
taken over each iteration. By default \code{chunk.size} is defined as
10000.}

\item{\code{verbose}}{A \link{logical} value to specify if more verbosity is
needed.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A \code{\link{FIterator}} object to transverse through
\code{\link{HDSubset}} instances
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-HDSubset-isBlinded"></a>}}
\if{latex}{\out{\hypertarget{method-HDSubset-isBlinded}{}}}
\subsection{Method \code{isBlinded()}}{
Checks if the subset contains a target class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{HDSubset$isBlinded()}\if{html}{\out{</div>}}
}

\subsection{Returns}{
A \link{logical} to specify if the subset contains a target class
or not.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-HDSubset-clone"></a>}}
\if{latex}{\out{\hypertarget{method-HDSubset-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{HDSubset$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
