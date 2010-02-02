\name{plot.hemo}

\alias{plot.hemo}

\title{Plot hemo data}

\description{Plot hemo data}

\usage{\method{plot}{hemo}(x, style=c("ts", "clock", "pairs"),
     which, cex=par("cex"), debug=FALSE, show.mean=TRUE, show.lm=TRUE, ...)}

\arguments{
  \item{x}{\code{hemo} object, typically result of \code{\link{read.hemo}}.}
  \item{style}{\code{"ts"} for time-series, \code{"clock"} for r-theta
    with angle indicating time of day, or \code{"pairs"} for a pairs
    plot.}
  \item{which}{integer, or list of integers, indicating what is to be
    plotted; see \dQuote{Details}.}
  \item{cex}{base symbol size to use; some elements are smaller than this.}
  \item{debug}{boolean flag, set to \code{TRUE} to get printout of some
    things that may help in debuggin the code.}
  \item{show.mean}{boolean flag, set to \code{TRUE} to get a ring with
    mean value.  This is only used for clock plots.}
  \item{show.lm}{boolean flag, set to \code{TRUE} to get a linear
    regression added to the plot (with confidence intervals on the
    prediction).}
  \item{\dots}{optional arguments passed to plotting functions.}
}

\details{Several plots are available, depending on the values of
  \code{item}, \code{style}, and \code{which}.  Values of \code{which}
  (for \code{item="bp"}) are as follows.
  \itemize{
    \item \code{which=1} for systolic pressure.
    \item \code{which=2} for diastolic pressure.
    \item \code{which=3} for mean arterial pressure.
    \item \code{which=4} for pulse pressure.
    \item \code{which=5} for pulse rate.
    \item \code{which=6} for weight.
    \item \code{which=7} for statistics.
  }
  If \code{which} is not provided, then the value \code{c(1,2,3,5,6)}
  will be used for \code{ts} plots,  \code{c(1,2,5,7)} for
  \code{clock} plots, or \code{c(2,3,6)} for \code{pairs} plots.
}

\examples{
library(hemo)
data(hemo)
plot(hemo, style="clock")
}

\seealso{See \code{\link{read.hemo}} for more information on dealing
  with the \code{hemo} data type, including examples.}

\references{
Some references from the literature are below.

http://hyper.ahajournals.org/cgi/content/full/30/6/1410

http://stroke.ahajournals.org/cgi/content/full/39/7/1932#R5-510677

http://hyper.ahajournals.org/cgi/reprint/20/5/601.pdf?ijkey=2b668babf1f9b647cd5fb79092cf825663db7d27

 "Hypertension: management of hypertension in adults in primary
 care". NICE Clinical Guideline 34. London, England: National Institute
 for Health and Clinical Excellence (NICE). June 2006. Retrieved 2008-09-15.  
 \url{http://www.nice.org.uk/nicemedia/pdf/CG034NICEguideline.pdf}
}

\author{Dan Kelley}
\keyword{misc}
