\name{summary}
\alias{summary.SK}

\title{
  Summary Method for SK Objects
}

\description{
  Returns (and prints) a summary list for \code{SK} objects.
}

\usage{
\method{summary}{SK}(object,
 \dots)
}

\arguments{
 \item{object}{A given object of the class \code{SK}.}
 \item{\dots}{Potential further arguments (required by generic).}
}

\author{
  José Cláudio Faria (\email{joseclaudio.faria@gmail.com})\cr
  Enio Jelihovschi (\email{eniojelihovs@gmail.com})\cr
  Ivan Bezerra Allaman (\email{ivanalaman@gmail.com})
}

\references{
  Chambers, J.M. and Hastie, T.J. (1992) \emph{Statistical Models in S}.
  Wadsworth and Brooks/Cole.
}

\seealso{
  \code{\link{SK}}
}

\examples{
  ##
  ## Examples: Completely Randomized Design (CRD)
  ## More details: demo(package='SK')
  ##
  
  data(CRD2)
  ## From: formula
  sk1 <- with(CRD2,
              SK(y ~ x,
                 data=dfm,
                 which='x',
                 id.trim=5))
  summary(sk1)

}

\keyword{package}

