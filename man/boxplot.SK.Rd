\name{boxplot.SK}
\alias{boxplot.SK}

\title{
  Boxplot SK Objects
}

\description{
  S3 method to plot \code{SK} objects.
}

\usage{
\method{boxplot}{SK}(x,
         mean.type   = c('line', 'point', 'none'),
         xlab        = NULL,
         mean.col    = 'gray',
         mean.pch    = 1,
         mean.lwd    = 1,
         mean.lty    = 1,
         args.legend = NULL, \dots)
}

\arguments{
  \item{x}{A \code{SK} object.}
  \item{mean.type}{The type of mean representation to be plotted. The default is \dQuote{line}.}
  \item{xlab}{A label for the \samp{x} axis.}
  \item{mean.col}{A vector of colors for the means representation.}
  \item{mean.pch}{A vector of plotting symbols or characters. Used only when \code{mean.type = 'point'}.}
  \item{mean.lwd}{Line width of mean.}
  \item{mean.lty}{Line type of the mean. Used only when \code{mean.type = 'line'}.}
  \item{args.legend}{List of additional arguments to be passed to \code{legend}; The default is \code{NULL}.}
  \item{\dots}{Optional plotting parameters.}
}

\details{
  The \code{boxplot.SK} function is an S3 method for plotting \code{SK}
  objects. It extends the generic \code{boxplot} function by overlaying
  Scott & Knott group labels above the plot frame and drawing the treatment
  mean within each box.
}

\author{
  Faria, J. C. (\email{joseclaudio.faria@gmail.com})\cr
  Jelihovschi, E. G. (\email{eniojelihovs@gmail.com})\cr
  Allaman, I. B. (\email{ivanalaman@gmail.com})
}

\references{
  Murrell, P. (2005) \emph{R Graphics}. Chapman and Hall/CRC Press.
}

\seealso{
  \code{\link{boxplot}}
}

\examples{
##
## Examples: Completely Randomized Design (CRD)
## More details: demo(package='SK')
##

library(ScottKnott)
data(CRD1)

## From: formula
## Simple
sk1 <- SK(y ~ x,
          data=CRD1$dfm,
          which='x')
boxplot(sk1)

## A little more elaborate
boxplot(sk1,
        mean.lwd=1.3,
        mean.col='red')

## More customisation
boxplot(sk1,
        mean.lwd=1.3,
        mean.lty=2,
        mean.col='red',
        args.legend=list(x='bottomleft'))

## With point type
boxplot(sk1,
        mean.type='point')

boxplot(sk1,
        mean.type='point',
        mean.pch=19,
        cex=1.5,
        mean.col='red')

## With a different point symbol
boxplot(sk1,
        mean.type='point',
        mean.pch='+',
        cex=2,
        mean.col='blue',
        args.legend=list(x='bottomleft'))
}

\keyword{package}
