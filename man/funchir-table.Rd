\name{funchir-table}
\alias{table2}
\alias{lyx.xtable}
\alias{lyx.texreg}
\alias{sanitize2}
\title{ Convenient Wrappers for creating and printing tables }
\description{
   Here are wrappers for common table creation/manipulation/printing operations.
}
\usage{
   table2(..., dig = if (prop) 2L else NULL, prop = FALSE, ord = FALSE, pct = FALSE)
   lyx.xtable(...)
   sanitize2(str)
}
\arguments{
  \item{...}{ For both functions, the code to be passed to the wrapped function. For \code{table2}, can also include arguments to \code{prop.table}. }
  \item{dig}{ Number of digits to be printed in the output. Defaults to 2 for proportion tables because output tends to be ugly if unrestricted. }
  \item{prop}{ Print a proportional table? This will activate a call to \code{prop.table}. }
  \item{ord}{ Should the table be ordered? Three inputs are understood: 1) \code{FALSE}; returns table in standard order, 2) \code{TRUE}; returns table in \emph{increasing} order, 3) \code{"dec"}; returns table in \emph{decreasing} order. }
  \item{pct}{ Should table values be converted to a percentage (i.e., multiplied by 100)? Typically used with \code{prop = TRUE}; note that \code{round} is applied \emph{after} percentage conversion, so keep this in mind if using \code{dig} simultaneously. }
  \item{str}{ \code{character} vector. }
}
\details{
  \code{table2} is especially useful for two common usages of \code{table}--producing ordered frequency tables of a variable (to find most/least common realizations) and producing proportional two-way tables of two variables.

  \code{lyx.xtable} (based on package \code{xtable}) is designed specifically for users of the free WYSIWYM LaTeX editor LyX, which converts copy-pasted LaTeX to indecipherable gibberish unless lines of code are separated by two newlines. Note that \code{lyx.xtable} wraps \code{xtable::print.xtable}.
  
  \code{sanitize2} is a replacement to the internal \code{sanitize} function used by default in \code{xtable}. Adds items for fixing left and right square brackets, which are (in the current--2017/03/03--version of \code{print.xtable} ) by default left alone, which can cause errors.
}

\seealso{ 
  \code{\link{table}}, \code{\link{prop.table}}
}
\examples{
  x <- sample(10, size = 100, replace = TRUE)
  y <- sample(3, size = 100, replace = TRUE)
  tbl <- table2(x, y, prop = TRUE, margin = 1)
  table2(x, ord = "dec")
  table2(y, ord = TRUE)
  table2(y, dig = -1L)
  
  \dontrun{
    lyx.xtable(xtable(tbl), sanitize.text.function = sanitize2)
  }
}