\name{print.SK}
\alias{print.SK}

\title{
	Print Method for \code{SK} objects.
}

\description{
	Returns (and prints) a list for objects of class \code{SK}.
}

\usage{

\method{print}{SK}(x, digits = 2L,...)

}

\arguments{
	\item{x}{A given object of the class \code{SK}.}
	\item{digits}{Minimal number of _significant_ digits. The default is 2.}
	\item{\dots }{Further arguments (require by generic).}
}

\author{
	Jose Claudio Faria (\email{joseclaudio.faria@gmail.com})\cr
	Enio G. Jelihovschi (\email{eniojelihovs@gmail.com})\cr
	Ivan Bezerra Allaman (\email{ivanalaman@gmail.com})\cr
}

\seealso{\code{\link{SK}}
}

\examples{
data(RCBD) 

sk <- with(RCBD,
       	   SK(y ~ blk + tra,
          		  data=dfm,
          		  which='tra'))
sk

}

\keyword{package}
\keyword{SK}
