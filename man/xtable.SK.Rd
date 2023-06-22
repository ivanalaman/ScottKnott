\name{xtable.SK}
\alias{xtable.SK}

\title{
  xtable method for \code{SK} objects.
}

\description{
  Convert an \code{SK} object to an \code{xtable.SK} object, which can then be printed as a LaTeX or HTML table. This function is an additional method to xtable function of \pkg{xtable} package.
}

\usage{

\method{xtable}{SK}(x, ...)

}

\arguments{
  \item{x}{A given object of the class \code{SK}.}
  \item{\dots}{Further arguments (require by xtable::xtable).}
}

\author{
  Jose Claudio Faria (\email{joseclaudio.faria@gmail.com})\cr  
  Enio G. Jelihovschi (\email{eniojelihovs@gmail.com})\cr
  Ivan Bezerra Allaman (\email{ivanalaman@gmail.com})\cr
}

\seealso{\code{\link{xtable}}
}

\examples{
data(RCBD) 
\dontrun{
  if(require(xtable)){
    lm1 <- with(RCBD,
                  lm(y ~ blk + tra,
                     data=dfm))

    sk1 <-  SK(lm1,
               which='tra')
    tb <- xtable(sk1)
    print(tb)
  }
 } 
}

\keyword{package}
\keyword{SK}
