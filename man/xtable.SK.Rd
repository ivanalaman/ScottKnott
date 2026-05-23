\name{xtable.SK}
\alias{xtable.SK}

\title{
  xtable method for \code{SK} objects.
}

\description{
  Convert an \code{SK} object to an \code{xtable.SK} object, which can then be printed as a LaTeX or HTML table. This function provides an additional method for the \code{xtable} function from the \pkg{xtable} package.
}

\usage{

\method{xtable}{SK}(x, ...)

}

\arguments{
  \item{x}{A given object of the class \code{SK}.}
  \item{\dots}{Further arguments (required by \code{xtable::xtable}).}
}

\author{
  Faria, J. C. (\email{joseclaudio.faria@gmail.com})\cr
  Jelihovschi, E. G. (\email{eniojelihovs@gmail.com})\cr
  Allaman, I. B. (\email{ivanalaman@gmail.com})\cr
}

\seealso{
 \code{\link[xtable:xtable]{xtable}}
}

\examples{
data(RCBD)

lm1 <- with(RCBD,
            lm(y ~ blk + tra,
               data=dfm))

sk1 <- SK(lm1,
          which='tra')

tb <- xtable(sk1)
\dontrun{
  print(tb)
}
}

\keyword{package}
