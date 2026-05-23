\name{plot.SK}
\alias{plot.SK}

\title{
  Plot SK objects
}

\description{
  S3 method to plot \code{SK} objects.
}

\usage{
\method{plot}{SK}(x,
     result         = TRUE,
     replicates     = TRUE,
     pch            = 19,
     col            = NULL,
     xlab           = NULL,
     ylab           = NULL,
     xlim           = NULL,
     ylim           = NULL,
     axisx          = TRUE,
     axisy          = TRUE,
     id.lab         = NULL,
     id.las         = 1,
     yl             = TRUE,
     yl.lty         = 3,
     yl.col         = 'gray',
     dispersion     = c('mm','sd','ci','cip'),
     d.lty          = 1,
     d.col          = 'black',
     title          = '', \dots)
}

\arguments{
  \item{x}{A \code{SK} object.}
  \item{result}{The result of the test (letters) should be visible.}
  \item{replicates}{The number of replicates should be visible.}
  \item{pch}{A vector of plotting symbols or characters.}
  \item{col}{A vector of colors for the means representation.}
  \item{xlab}{A label for the \samp{x} axis.}
  \item{ylab}{A label for the \samp{y} axis.}
  \item{xlim}{The \samp{x} limits of the plot.}
  \item{ylim}{The \samp{y} limits of the plot.}
  \item{axisx}{If \code{TRUE}, the x axis is drawn using defaults; set to \code{FALSE} to suppress it.}
  \item{axisy}{If \code{TRUE}, the y axis is drawn using defaults; set to \code{FALSE} to suppress it.}
  \item{id.lab}{Factor level names at \samp{x} axis.}
  \item{id.las}{Factor level names written either horizontally or vertically.}
  \item{yl}{Horizontal (reference) line connecting the circle to the \samp{y} axis.}
  \item{yl.lty}{Line type of \samp{yl}.}
  \item{yl.col}{Line color of \samp{yl}.}
  \item{dispersion}{Type of dispersion bar drawn through each mean point. Options: \samp{mm} (min-max range), \samp{sd} (standard deviation), \samp{ci} (individual confidence interval), \samp{cip} (pooled confidence interval). Default is \samp{mm}.}
  \item{d.lty}{Line type of dispersion.}
  \item{d.col}{A vector of colors for the line type of dispersion.}
  \item{title}{A title for the plot.}
  \item{\dots}{Optional plotting parameters.}
}

\details{
  The \code{plot.SK} function is an S3 method for plotting \code{SK} objects.
  It generates a series of points representing the treatment means, optionally
  with vertical dispersion bars. The \samp{ci} option is calculated using each
  treatment's own variance as an estimate of the population variance. The
  \samp{cip} option is calculated using the mean square error (MSE) as an
  estimate of the population variance.
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
  \code{\link{plot}}
}

\examples{
##
## Examples: Completely Randomized Design (CRD)
## More details: demo(package='ScottKnott')
##

library(ScottKnott)
data(CRD2)

## From: formula
sk1 <- with(CRD2,
            SK(y ~ x,
               data=dfm,
               which='x'))

old.par <- par(mar=c(6, 3, 6, 2))
plot(sk1,
     id.las=2)

plot(sk1,
     yl=FALSE,
     dispersion='sd',
     id.las=2)

## From: aov
av <- with(CRD2,
           aov(y ~ x,
               data=dfm))
summary(av)

sk2 <- SK(x=av,
          which='x')

col=c(rep(2, 6),
      rep(3, 36),
      rep(4, 1),
      rep(5, 2))

plot(sk2,
     dispersion='sd',
     yl=FALSE,
     id.las=2,
     col=col,
     d.col=col)

## From: lm
av_lm <- with(CRD2,
           lm(y ~ x,
              data=dfm))

sk3 <- SK(x=av_lm,
          which='x')

par(mfrow=c(2, 1))
plot(sk2,
     dispersion='ci',
     yl=FALSE,
     id.las=2,
     col=col,
     d.col=col)

plot(sk2,
     dispersion='cip',
     yl=FALSE,
     id.las=2,
     col=col,
     d.col=col)

par(mfrow=c(1, 1))
par(old.par)
}

\keyword{package}
