\name{read.hemo}

\alias{read.hemo}

\title{Read a hemo data file}

\description{Read a hemo data file}

\usage{read.hemo(file, debug=FALSE)}

\arguments{
  \item{file}{a connection or a character string giving the name of the
    file to load.}
  \item{debug}{a flag that can be set to \code{TRUE} to turn on debugging.}
}

\details{Each line of the data file is taken to be a recording of a
  reading from an instrument, or an event.  Elements on these lines are
  separated with white space (space or tab).  The first element is an
  identifier, the second is a date in the format \code{YYYY-MM-DD}, and
  the third is a time in the format \code{HH:MM}.  After that, any
  number of values may be given.

  At present, the only identifier recognized is \code{BP}, for blood
  pressure.  This has three values following the time: the systolic
  pressure, in mm of Hg, the diastolic pressure in mm of Hg, and the
  pulse rate in beats per minute.  For example, the line \code{BP
  2010-01-30 0806 110 80 55} indicates systolic and diastolic pressures
  of 110 and 80, respectively, and a pulse of 55 beats per minute.
}

\value{An object of \code{\link[base]{class}} \code{"hemo"}.}

\seealso{Objects of type \code{hemo} can be plotted
  with \code{\link{plot.hemo}}, and summarized with
  \code{\link{summary.hemo}}, both of which are generic functions.
}

\author{Dan Kelley}

\keyword{misc}
