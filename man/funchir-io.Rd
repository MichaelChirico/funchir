\name{funchir-io}
\alias{abbr_to_colClass}
\title{ Convenient Functions for Data Reading }
\description{
   Functions which come in particular handy for process of reading in data which can turn verbose code into readable, clean code.
}
\usage{
   abbr_to_colClass(inits, counts)
}
\arguments{
  \item{inits}{ Initials of data types to be passed to a \code{colClasses} argument (most typically in \code{fread} from \code{data.table} for me). See details. }
  \item{counts}{ Corresponding counts (as an \emph{unbroken string}) of each type given in \code{inits}. See details }
}
\details{
  \code{abbr_to_colClass} was designed specifically for reading in large (read: wide, i.e., with many fields) data files when it is also necessary to specify the types to expect to the reader for speed or for accuracy.

  Currently recognized types are \code{blank}, \code{character}, \code{factor}, \code{logical}, \code{integer}, \code{numeric}, \code{Date}, \code{date}, \code{text} and \code{skip}, which are abbreviated to their first initials: \code{"b"}, \code{"c"}, \code{"f"}, \code{"l"}, \code{"i"}, \code{"n"}, \code{"D"}, \code{"d"}, \code{"t"} and \code{"s"}, respectively.

  Since like types are often found in sequence, the \code{counts} argument can condense the call considerably--if three integer columns appear in a row, for example, we could specify \code{inits="i"} and \code{counts="3"} instead of the breathier \code{inits="iii"}, \code{counts="111"}.

  Note that since \code{counts} is read digit-by-digit, sequences of length greater than 9 must be broken up into size-9 (or smaller) chunks, e.g., if there are 20 \code{Date} fields in a row, we could set \code{inits="ddd"}, \code{counts="992"}. This approach was taken (rather than, say, requiring \code{counts} to be an integer vector of counts) as I find it speedier and more concise, and the direct parallel to \code{inits} can elucidate issues which arise directly in the code instead of, say, checking \code{cbind(strsplit(inits, split = "")[[1L]], counts)}.
}

\examples{
  abbr_to_colClass(inits = "ncifdfd", counts = "1234567")
}
