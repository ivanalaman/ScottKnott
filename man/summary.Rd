\name{summary}
\alias{summary.SK}
\alias{summary.SK.nest}

\title{
  Summary Method for SK and SK.nest Objects
}

\description{
  Returns (and prints) a summary list for \code{SK} and \code{SK.nest} objects.
}

\usage{
  \method{summary}{SK}(object, \dots)
  \method{summary}{SK.nest}(object, \dots)
}

\arguments{
  \item{object}{A given object of the class \code{SK} or \code{SK.nest}.}
  \item{\dots}{Potential further arguments (require by generic).}
}

\author{
  Enio Jelihovschi (\email{eniojelihovs@gmail.com})\cr
  Jose Claudio Faria (\email{joseclaudio.faria@gmail.com})\cr
  Sergio Oliveira (\email{solive@uesc.br})\cr
}

\references{
  Chambers, J. M. and Hastie, T. J. 1992. Statistical Models in S.
  Wadsworth & Brooks/Cole.
}

\seealso{\code{\link{ScottKnott}}}

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

  ## Nested: p1/N
  nsk1 <- with(FE, SK.nest(x=dm, y=y, model='y ~ blk + N*P*K',
                           which='N:P', fl2=1))
  summary(nsk1)

  ## Nested: k2/p2/N
  nsk2 <- with(FE, SK.nest(x=dm, y=y, model='y ~ blk + N*P*K',
                           which='N:P:K', fl2=2, fl3=2))
  summary(nsk2)

  ## Nested: k1/n1/P
  nsk3 <- with(FE, SK.nest(x=dm, y=y, model='y ~ blk + P*N*K',
                           which='P:N:K', fl2=1, fl3=1))
  summary(nsk3)

  ## Nested: p1/n1/K
  nsk4 <- with(FE, SK.nest(x=dm, y=y, model='y ~ blk + K*N*P',
                           which='K:N:P', fl2=1, fl3=1))
  summary(nsk4)

  ##
  ## Example: Split-plot Experiment (SPE)
  ## More details: demo(package='ScottKnott')
  ##
  
  ## Note: The factors are in uppercase and its levels in lowercase!

  data(SPE)

  ## The parameters can be: design matrix and the response variable,
  ## data.frame or aov
  
  ## From: design matrix (dm) and response variable (y)
  ## Main factor: P
  sk1 <- with(SPE, SK(x=dm, y=y, model='y ~ blk + SP*P + Error(blk/P)',
                      which='P', error ='blk:P'))
  summary(sk1)

  ## Nested: p1/SP
  skn1 <- with(SPE, SK.nest(x=dm, y=y, model='y ~ blk + SP*P + Error(blk/P)',
                            which='SP:P', error ='Within', fl2=1 ))
  summary(skn1)

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
  
  ## Nested: p1/sp
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

