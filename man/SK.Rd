\name{SK}
\alias{SK}
\alias{SK.default}
\alias{SK.aov}
\alias{SK.aovlist}

\title{
  The ScottKnott Clustering Algoritm for Single Experiments
}

\description{
  These are methods for objects of class \code{vector}, \code{matrix} or
  \code{data.frame} joined as default, \code{aov} and \code{aovlist} for
  single experiments.
}

\usage{
  \method{SK}{default}(x, y=NULL, model, which, id.trim=3, error, sig.level=.05, \dots)
  \method{SK}{aov}(x, which=NULL, id.trim=3, sig.level=.05, \dots)
  \method{SK}{aovlist}(x, which, id.trim=3, error, sig.level=.05, \dots)  
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
  \item{error}{The error to be considered.}
  \item{sig.level}{Level of Significance used in the SK algorithm to create
    the groups of means. The default value is 0.05.}
  \item{\dots}{Potential further arguments (require by generic).}    
}

\details{
  The function \code{SK} returns an object of class \code{SK} 
  respectivally containing the groups of means plus other
  necessary variables for summary and plot.
  
  The generic functions \code{summary} and \code{plot} are used to obtain and
  print a summary and a plot of the results.
}

\value{
  The function \code{SK} returns a list of the class \code{SK} with the slots:
  \item{av}{A \code{list} storing the result of \code{aov}.}
  \item{groups}{A vector of length equal the number of factor levels marking the groups generated.}
  \item{nms}{A vector of the labels of the factor levels.}
  \item{ord}{A vector which keeps the position of the means of the factor levels in decreasing order.}
  \item{m.inf}{A matrix which keeps the means, minimum and maximum of the factor levels in decreasing order.}
  \item{sig.level}{A vector of length 1 giving the level of significance of the test.}
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
  ## Examples: Completely Randomized Design (CRD)
  ## More details: demo(package='ScottKnott')
  ##
  
  ## The parameters can be: vectors, design matrix and the response variable,
  ## data.frame or aov
  data(CRD2)
  
  ## From: design matrix (dm) and response variable (y)
  sk1 <- with(CRD2, SK(x=dm, y=y, model='y ~ x',
                       which='x', sig.level=0.005, id.trim=5))
  summary(sk1)
  plot(sk1, col=rainbow(max(sk1$groups)), mm.lty=3, id.las=2, rl=FALSE,
       title='factor levels, sig.level=0.005')
  
  ## From: data.frame (dfm)
  sk2 <- with(CRD2, SK(x=dfm, model='y ~ x',
                       which='x', id.trim=5))
  summary(sk2)
  plot(sk2, col=rainbow(max(sk2$groups)), id.las=2, rl=FALSE)
  
  ## From: aov
  av <- with(CRD2, aov(y ~ x , data = dfm))
  summary(av)
  
  sk3 <- with(CRD2, SK(x=av,
                       which='x', id.trim=5))
  summary(sk3)
  plot(sk3, col=rainbow(max(sk3$groups)), rl=FALSE, id.las=2, title=NULL)

  ##
  ## Example: Randomized Complete Block Design (RCBD)
  ## More details: demo(package='ScottKnott')
  ##
  
  ## The parameters can be: design matrix and the response variable,
  ## data.frame or aov
  
  data(RCBD)
  
  ## Design matrix (dm) and response variable (y)
  sk1 <- with(RCBD, SK(x=dm, y=y, model='y ~ blk + tra',
                       which = 'tra'))
  summary(sk1)
  plot(sk1)
  
  ## From: data.frame (dfm), which='tra'
  sk2 <- with(RCBD, SK(x=dfm, model='y ~ blk + tra',
                       which='tra'))
  summary(sk2)
  plot(sk2, mm.lty=3, title='Factor levels')
  
  ##
  ## Example: Latin Squares Design (LSD)
  ## More details: demo(package='ScottKnott')
  ##
  
  ## The parameters can be: design matrix and the response variable,
  ## data.frame or aov
  
  data(LSD)
  
  ## From: design matrix (dm) and response variable (y)
  sk1 <- with(LSD, SK(x=dm, y=y, model='y ~ rows + cols + tra',
                      which='tra'))
  summary(sk1)
  plot(sk1)
  
  ## From: data.frame
  sk2 <- with(LSD, SK(x=dfm, model='y ~ rows + cols + tra',
                      which='tra'))
  summary(sk2)
  plot(sk2, title='Factor levels')
  
  ## From: aov
  av <- with(LSD, aov(y ~ rows + cols + tra, data=dfm))
  summary(av)
  
  sk3 <- SK(av,
            which='tra')
  summary(sk3)
  plot(sk3, title='Factor levels')

  ##
  ## Example: Factorial Experiment (FE)
  ## More details: demo(package='ScottKnott')
  ##
  
  ## The parameters can be: design matrix and the response variable,
  ## data.frame or aov
  
  ## Note: The factors are in uppercase and its levels in lowercase!

  data(FE)
  ## From: design matrix (dm) and response variable (y)
  ## Main factor: N
  sk1 <- with(FE, SK(x=dm, y=y, model='y ~ blk + N*P*K', 
                     which='N'))
  summary(sk1)
  plot(sk1, title='Main effect: N')

  ## Nested: p1/N
  nsk1 <- with(FE, SK.nest(x=dm, y=y, model='y ~ blk + N*P*K',
                           which='N:P', fl2=1))
  summary(nsk1)
  plot(nsk1, title='Effect: p1/N')
}

\keyword{package}
\keyword{htest}
\keyword{univar}
\keyword{tree}
\keyword{design}

