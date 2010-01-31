\name{summary.hemo}

\alias{summary.hemo}
\alias{print.summary.hemo}

\title{Summarize a hemo object}

\description{Summarize a hemo object}

\usage{
\method{summary}{hemo}(object, \dots)
\method{print}{summary.hemo}(x, digits=max(6, getOption("digits")-1), \dots)
}

\arguments{
  \item{object}{an object of class \code{"pt"}, usually, a result of a
    call to \code{read.pt}.}
  \item{x}{an object of class \code{"summary.pt"}, usually, a result
    of a call to \code{summary.pt}.}
  \item{digits}{the number of significant digits to use when printing.}
  \item{\dots}{further arguments passed to or from other methods.}
}

\details{Pertinent summary information is presented.}

\value{An object of class \code{"summary.hemo"}, which contains
  pertinent information about the TDR record and its processing.}

\seealso{...}

\examples{
library(hemo)
data(hemo)
summary(hemo)
}

\author{Dan Kelley}

\keyword{misc}
