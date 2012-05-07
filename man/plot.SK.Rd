\name{plot.SK}
\alias{plot.SK}

\title{
  Plot SK and SK.nest Objects
}

\description{
  S3 method to plot \code{SK} and \code{SK.nest} objects.
}

\usage{
  \method{plot}{SK}(x, pch=19, col=NULL,
    xlab=NULL, ylab=NULL, xlim=NULL, ylim=NULL,
    id.lab=NULL, id.las=1, id.col=TRUE,
    rl=TRUE, rl.lty=3, rl.col='gray',
    mm=TRUE, mm.lty=1, 
    title="Means grouped by color(s)", \dots)
}

\arguments{
  \item{x}{A \code{SK} object.}
  \item{pch}{A vector of plotting symbols or characters.}
  \item{col}{A vector of colors for the means representation.}
  \item{xlab}{A label for the x axis.}
  \item{ylab}{A label for the y axis.}
  \item{xlim}{The x limits of the plot}.
  \item{ylim}{The y limits of the plot.}
  \item{id.lab}{Factor level names at x-axis.}
  \item{id.las}{Factor level names written either horizontally or vertically.}
  \item{id.col}{A logical value. If TRUE (the default), the \code{col} parameter will be used for the x axis.}
  \item{rl}{Horizontal line connecting the circle to the y-axis.}
  \item{rl.lty}{Line type of rl.}
  \item{rl.col}{Line color of rl.}
  \item{mm}{Vertical line through the circle (mean value) linking the minimum to the maximum of the factor level values corresponding to that mean value.}
  \item{mm.lty}{Line type of mm.}
  \item{title}{A title for the plot.}
  \item{\dots}{Optional plotting parameters.}
} 

\details{
	The \code{plot.SK} function is a S3 method to plot \code{SK} and
  \code{SK.nest} objetcs. It generates a serie of points (the means) and a 
  vertical line showing the minimum e maximum of the values corresponding to
  each group mean. The groups are diferentiated by colors.
}

\author{
  Enio Jelihovschi (\email{eniojelihovs@gmail.com})\cr
  Jose Claudio Faria (\email{joseclaudio.faria@gmail.com})\cr
  Sergio Oliveira (\email{solive@uesc.br})\cr
}

\references{
  Murrell, P. 2005. R Graphics. Chapman & Hall/CRC Press.
}

\seealso{\code{\link{plot}}}

\examples{
  ##
  ## Examples: Completely Randomized Design (CRD)
  ## More details: demo(package='ScottKnott')
  ##

  library(ScottKnott)
  data(CRD2)

  ## From: vectors x and y
  sk1 <- with(CRD2, SK(x=x, y=y, model='y ~ x',
                       which='x', id.trim=5))
  plot(sk1, id.las=2, rl=FALSE, title='Factor levels', sig.level=.05)

  ## From: design matrix (dm) and response variable (y)
  sk2 <- with(CRD2, SK(x=dm, y=y, model='y ~ x',
                       which='x', sig.level=0.005, id.trim=5))
  plot(sk2, col=rainbow(max(sk2$groups)), mm.lty=3, id.las=2, rl=FALSE,
       title='Factor levels', sig.level=0.005, )

  ## From: data.frame (dfm)
  sk3 <- with(CRD2, SK(x=dfm, model='y ~ x',
                       which='x', id.trim=5))
  plot(sk3, col=rainbow(max(sk3$groups)), id.las=2, id.col=FALSE, rl=FALSE)

  ## From: aov
  av <- with(CRD2, aov(y ~ x , data = dfm))
  summary(av)

  sk4 <- with(CRD2, SK(x=av,
                       which='x', id.trim=5))
  plot(sk4, col=rainbow(max(sk4$groups)), rl=FALSE, id.las=2, id.col=FALSE,
       title=NULL)
}

\keyword{package}
\keyword{htest}
\keyword{univar}
\keyword{tree}
\keyword{design}

