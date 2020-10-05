\name{SK}
\alias{SK}
\alias{SK.default}
\alias{SK.aov}
\alias{SK.lm}
\alias{SK.formula}
\alias{SK.aovlist}
\alias{SK.lmerMod}

\title{
  The SK Test for Single Experiments
}

\description{
  These are methods for objects of class \code{formula}, \code{lm}, \code{aov}, \code{aovlist} and \code{lmerMod} for single, factorial, split-plot and split-split-plot experiments.
}

\usage{
SK(x,...)

\method{SK}{formula}(formula,
       data            = NULL,
       which           = NULL,
       fl1             = NULL,
       fl2             = NULL,
       error           = NULL,
       sig.level       = .05,
       round           = 2,
       \dots)

\method{SK}{lm}(x,
       which           = NULL,
       fl1             = NULL,
       fl2             = NULL,
       error           = NULL,
       sig.level       = .05,
       round           = 2,
       \dots)

\method{SK}{aovlist}(x,
       which           = NULL,
       fl1             = NULL,
       fl2             = NULL,
       error           = NULL,
       sig.level       = .05,
       round           = 2,
       \dots)

\method{SK}{lmerMod}(x,
       which           = NULL,
       fl1             = NULL,
       fl2             = NULL,
       error           = NULL,
       sig.level       = .05,
       round           = 2,
       \dots)

}

\arguments{
  \item{x,formula}{A \code{formula}, \code{lm}, \code{aov}, \code{aovlist} and \code{lmerMod} class object. Objects of the \code{formula} class follow ``response variable ~ predicted variable.}
  \item{data}{A object of the \code{data.frame} class. Use only objects of \code{formula} class.}
  \item{which}{The name of the treatment to be used in the comparison.
    The name must be inside quoting marks.}
  \item{fl1}{A vector of length 1 giving the level of the first factor in nesting order tested.}
  \item{fl2}{A vector of length 1 giving the level of the second factor in nesting order tested.} 
  \item{error}{The error to be considered. If from experiment at split plot or split-split plot pay attention! See details!}
  \item{sig.level}{Level of Significance used in the SK algorithm to create the groups of means. The default value is 0.05.}
  \item{round}{Integer indicating the number of decimal places.}
  \item{\dots}{Potential further arguments (required by generic).}
}

\details{
  The function \code{SK} returns an object of class \code{SK} containing the groups of means plus other necessary variables for summary and plot.

  The generic functions \code{summary} and \code{plot} are used to obtain and print a summary and a plot of the results.

  The error arguments may be used whenever the user want a specific error other than the experimental error. At the split plot and split-split plot experiment, combination of error may be specified with "/" in the sequence of the \code{which} argument. For example, a object of \code{aovlist} class, a possible combination would be \code{error = 'Within/blk:plot'} at case block split plot experiment with \code{which = 'subplot:plot'} argument.
}

\value{
  The function \code{SK} returns a list of the class \code{SK} with the slots:
  \item{out}{A \code{list} storing the result of Scott & Knott test.}
  \item{info}{A \code{list} storing the descriptive statistics.} 
  \item{stat}{A \code{matrix} with the statistics of each clustering process.}
  \item{clus}{A \code{list} with the groups formed in each clustering process.}
}

\author{
  José Cláudio Faria (\email{joseclaudio.faria@gmail.com})\cr
  Enio Jelihovschi (\email{eniojelihovs@gmail.com})\cr
  Ivan Bezerra Allaman (\email{ivanalaman@gmail.com})
}

\references{
  Miller, R.G. (1981) \emph{Simultaneous Statistical Inference}. Springer.

  Ramalho M.A.P, Ferreira D.F and Oliveira A.C. (2000) \emph{Experimentação em Genética
    e Melhoramento de Plantas}. Editora UFLA.

  Steel, R.G., Torrie, J.H and Dickey D.A. (1997) \emph{Principles and procedures of statistics:
    a biometrical approach}. Third Edition.

  Yandell, B.S. (1997) \emph{Practical Data Analysis for Designed Experiments}. 
  Chapman and Hall.
}

\examples{
##
## Examples:Randomized Complete Block Design (RCBD)
## More details: demo(package='SK')
##

## The parameters can be: formula, aov, lm, aovlist and lmerMod

data(RCBD)

## From: formula
sk1 <- with(RCBD,
            SK(y ~ blk + tra,
               data=dfm,
               which='tra'))
summary(sk1)

## From: merMod
## This class is specific of the lme4 package.
\dontrun{
  if(require(lme4)){
    lmer1 <- with(RCBD,
                  lmer(y ~ (1|blk) + tra,
                       data=dfm))

    sk2 <-  SK(lmer1,
               which='tra')
    summary(sk2)
  }
}
##
## Example: Latin Squares Design (LSD)
## More details: demo(package='SK')
##

data(LSD)

## From: formula
sk3 <- with(LSD,
            SK(y ~ rows + cols + tra,
               data=dfm,
               which='tra'))
summary(sk3)

## From: aov
av1 <- with(LSD,
            aov(y ~ rows + cols + tra,
                data=dfm))

sk4 <- SK(av1,
              which='tra')
summary(sk4)

## From: lm
lm1 <- with(LSD,
            lm(y ~ rows + cols + tra,
               data=dfm))

sk5 <- SK(lm1,
          which='tra')
summary(sk5)

##
## Example: Factorial Experiment (FE)
## More details: demo(package='SK')
##

data(FE)
## From: formula
## Main factor: N
sk6 <- with(FE,
            SK(y ~ blk + N*P*K,
               data=dfm,
               which='N'))
summary(sk6)

## Nested: p1/N
# From: formula
n_sk1 <- with(FE,
              SK(y ~ blk + N*P*K,
                 data=dfm,
                 which='P:N',
                 fl1=1))
summary(n_sk1) 

## Nested: p2/N
# From: lm
lm2 <- with(FE,
            lm(y ~ blk + N*P*K, 
               dfm))

n_sk2 <- with(FE,
              SK(lm2,
                 which='P:N',
                 fl1=2))
summary(n_sk2) 

## Nested: n1/P
# From: aov
av2 <- with(FE,
            aov(y ~ blk + N*P*K,
                dfm))

n_sk3 <- with(FE,
              SK(av2,
                 which='N:P',
                 fl1=1))
summary(n_sk3) 

# From: merMod
\dontrun{
  if(require(lme4)){
    lmer2 <- with(FE,
                  lmer(y ~ (1|blk) + N*P*K,
                       dfm))

    n_sk4 <- with(FE,
                  SK(lmer2,
                     which='N:P',
                     fl1=1))
    summary(n_sk4)
  }
}

##
## Example: Split-plot Experiment (SPET)
## More details: demo(package='SK')
##
data(SPET)

## From lm
lm3 <- with(SPET,
            lm(y ~ blk*tra + tra*year,
               dfm))

# crotgrantiana/year
sp_sk1 <- SK(lm3,
             which='tra:year',
             fl1=1)
summary(sp_sk1) 

# year1/tra
# It is necessary to set year error with trat error in the order of the "which" argument.
# It is necessary to inform how to combinate the errors
sp_sk2 <-  SK(lm3,
              which='year:tra',
              error='Residuals/blk:tra',
              fl1=1)
summary(sp_sk2)

# From merMod
# Onty tra
\dontrun{
  if(require(lme4)){
    lmer3 <- with(SPET,
                  lmer(y ~ blk + (1|blk:tra) + tra*year,
                       dfm))

    # comparison only tra
    sp_sk3 <- SK(lmer3,
                 which = 'tra',
                 error = 'blk:tra')
    summary(sp_sk3)

    # year1/tra
    sp_sk4 <- SK(lmer3,
                 which='year:tra',
                 error='Residual/blk:tra',
                 fl1=1)
    summary(sp_sk4)
  }
}

## Example: Split-split-plot Experiment (SSPE)
## More details: demo(package='SK')
##

data(SSPE)
## From: formula
## Main factor: P
## It is necessary to inform the appropriate error for the test
ssp_sk1 <- with(SSPE,
                SK(y ~ blk + P*SP*SSP + Error(blk/P/SP),
                   data=dfm,
                   which='P',
                   error='blk:P'))
summary(ssp_sk1)

## Main factor: SP
## It is necessary to inform the appropriate error for the test
ssp_sk2 <- with(SSPE,
                SK(y ~ blk + P*SP*SSP + Error(blk/P/SP),
                   data=dfm,
                   which='SP',
                   error='blk:P:SP'))
summary(ssp_sk2)

## Main factor: SSP
ssp_sk3 <- with(SSPE,
                SK(y ~ blk + P*SP*SSP + Error(blk/P/SP),
                   data=dfm,
                   which='SSP'))
summary(ssp_sk3)

## From: aov
## Main factor: SSP
av3 <- with(SSPE,
            aov(y ~ blk + P*SP*SSP + Error(blk/P/SP),
                data=dfm))

ssp_sk4 <- SK(av3,
              which='SSP')
summary(ssp_sk4)

## Nested: p1/SP
## It is necessary to inform the appropriate error for the test
ssp_sk5 <- SK(av3,
              which='P:SP',
              error='blk:P:SP',
              fl1=1)
summary(ssp_sk5)

## Nested: p1/SSP
ssp_sk6 <- SK(av3,
              which='P:SSP',
              fl1=1)
summary(ssp_sk6)

## Nested: p1/sp1/SSP
## Testing SSP inside of level one of P and level one of SP
ssp_sk7 <- SK(av3,
              which='P:SP:SSP',
              fl1=1,
              fl2=1)
summary(ssp_sk7)

## Nested: p2/sp1/SSP
ssp_sk8 <- SK(av3,
              which='P:SP:SSP',
              fl1=2,
              fl2=1)
summary(ssp_sk8)

## Nested: sp1/P
## It is necessary to inform the appropriate error for the test
ssp_sk9 <- SK(av3,
              which='SP:P',
              error='blk:P:SP/blk:P',
              fl1=1)

summary(ssp_sk9)

## Nested: ssp1/SP
ssp_sk10 <- SK(av3,
               which='SSP:SP',
               error='Within/blk:P:SP',
               fl1=1)
summary(ssp_sk10)

## Nested: ssp1/sp1/P
## It is necessary to inform the appropriate error for the test
ssp_sk11 <- SK(av3,
               which='SSP:SP:P',
               error='Within/blk:P:SP/blk:P',
               fl1=1,
               fl2=1)
summary(ssp_sk11)

## UNBALANCED DATA
## The average are adjusted by "Least-Square-Means" methodology.
## From: formula
data(CRD2)

uCRD2 <- CRD2$dfm
uCRD2[c(3, 5, 10, 44, 45), 3] <- NA

usk1 <-  SK(y ~ x,
            data=uCRD2,
            which='x')
summary(usk1)

## From: lm
ulm1 <- lm(y ~ x,
           data=uCRD2)

usk2 <- SK(ulm1,
           which='x')
summary(usk2)

## Factorial Experiments
## Nested: p1/N
# From: lm

uFE <- FE$dfm
uFE[c(3, 6, 7, 20, 31, 32), 5] <- NA

ulm2 <- lm(y ~ blk + N*P*K,
           uFE)

## Nested: p1/N
usk3 <- SK(ulm2,
           data=uFE,
           which='P:N',
           fl1=1)
summary(usk3) 

## Nested: p2/n2/K
usk4 <- SK(ulm2,
           data=uFE,
           which='P:N:K',
           fl1=2,
           fl2=2)
summary(usk4) 
}

\keyword{package}
