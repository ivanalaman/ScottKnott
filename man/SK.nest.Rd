\name{SK.nest}
\alias{SK.nest}
\alias{SK.nest.default}
\alias{SK.nest.aov}
\alias{SK.nest.aovlist}

\title{
  The ScottKnott Clustering Algoritm for Factorial, Split-plot and Split-SPlit
    plot Experiments
}

\description{
  These are methods for objects of class \code{vector}, \code{matrix} or
  \code{data.frame} joined as default, \code{aov} and \code{aovlist} for
  factorial, split-plot and split-split-plot experiments.
}

\usage{
  \method{SK.nest}{default}(x, y=NULL, model, which, id.trim=3, error, fl2, fl3=0, sig.level=.05, \dots)
  \method{SK.nest}{aov}(x, which, id.trim=3, fl2, fl3=0, sig.level=.05, \dots)
  \method{SK.nest}{aovlist}(x, which, id.trim=3, error, fl2, fl3=0, sig.level=.05, \dots)
}

\arguments{
  \item{x}{A design matrix, \code{data.frame} or an \code{aov} object.}
  \item{y}{A vector of response variable. It is necessary to inform this
    parameter only if \code{x} represent the design matrix.}
  \item{which}{The name of the treatment to be used in the comparison.
    The name must be inside quoting marks.}
  \item{model}{If x is a \code{data.frame} object, the model to be used in the
    aov must be specified.}
  \item{id.trim}{The number of character to trim the id label.}
  \item{fl2}{A vector of length 1 giving the level of the second factor in nesting order tested.}
  \item{fl3}{A vector of length 1 giving the level of the third factor in nesting order tested.}
  \item{error}{The error to be considered, only in case of split-plots experiments.}
  \item{sig.level}{Level of Significance used in the SK algorithm to create
    the groups of means. The default value is 0.05.}
  \item{\dots}{Potential further arguments (require by generic).}
}

\details{
  The function \code{SK.nest} returns an object of class 
  \code{SK.nest} containing the groups of means plus other
  necessary variables for summary and plot.
  
  The generic functions \code{summary} and \code{plot} are used to obtain and
  print a summary and a plot of the results.
}

\value{
  The function \code{SK.nest} returns a list of the class \code{SK.nest} with the slots:
  \item{av}{A \code{list} storing the result of \code{aov}.}
  \item{groups}{A vector of length equal the number of factor levels marking the groups generated.}
  \item{nms}{A vector of the labels of the factor levels.}
  \item{ord}{A vector which keeps the position of the means of the factor levels in decreasing order.}
  \item{m.inf}{A matrix which keeps the means, minimum and maximum of the factor levels in decreasing order.}
  \item{sig.level}{A vector of length 1 giving the level of significance of the test.}
  \item{r}{A vector of length 1 giving the number of replicates.}
  \item{which}{The name of the factor whose levels were tested.}
  \item{tab}{An array keeping the names of the factors and factor levels and also the mean value of the repetitions for every combination of factor levels.} 
  \item{fl2}{A vector of length 1 giving the level of the second factor in nesting order tested.}
  \item{fl3}{A vector of length 1 giving the level of the third factor in nesting order tested.}
}

\author{
  Enio Jelihovschi (\email{eniojelihovs@gmail.com})\cr
  Jose Claudio Faria (\email{joseclaudio.faria@gmail.com})\cr
  Sergio Oliveira (\email{solive@uesc.br})\cr
}

\references{
  Ramalho M.A.P., Ferreira D.F., Oliveira A.C. 2000. \emph{Experimentacao em Genetica
  e Melhoramento de Plantas}. Editora UFLA.

  Scott R.J., Knott M. 1974. A cluster analysis method for grouping mans in the
  analysis of variance. \emph{Biometrics}, \bold{30}, 507-512.
}

\examples{
  ##
  ## Example: Split-split-plot Experiment (SSPE)
  ## More details: demo(package='ScottKnott')
  ##
  
  ## Note: The factors are in uppercase and its levels in lowercase!

  data(SSPE)
  ## From: design matrix (dm) and response variable (y)
  ## Main factor: P
  sk1 <- with(SSPE, SK(dm, y, model='y ~ blk + SSP*SP*P + Error(blk/P/SP)',
                       which='P', error='blk:P'))
  summary(sk1)
  
  # Main factor: SP
  sk2 <- with(SSPE, SK(dm, y, model='y ~ blk + SSP*SP*P + Error(blk/P/SP)',
                       which='SP', error='blk:P:SP', sig.level=0.025))
  summary(sk2)
  
  # Main factor: SSP
  sk3 <- with(SSPE, SK(dm, y, model='y ~ blk + SSP*SP*P + Error(blk/P/SP)',
                       which='SSP', error='Within', sig.level=0.1))
  summary(sk3)
  
  ## Nested: p1/SP
  skn1 <- with(SSPE, SK.nest(dm, y, model='y ~ blk + SSP*SP*P + Error(blk/P/SP)',
                             which='SP:P', error='blk:P:SP', fl2=1))
  summary(skn1)

  ## From: aovlist
  av <- with(SSPE, aov(y ~  blk + SSP*SP*P + Error(blk/P/SP), data=dfm))
  summary(av)   

  ## Nested: p/sp/SSP (at various levels of SP and P) 
  skn2 <- SK.nest(av, which='SSP:SP:P', error='Within', fl2=1, fl3=1)
  summary(skn2)

  skn3 <- SK.nest(av, which='SSP:SP:P', error='Within', fl2=2, fl3=1)
  summary(skn3)
}

\keyword{package}
\keyword{htest}
\keyword{univar}
\keyword{tree}
\keyword{design}

