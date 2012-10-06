\name{ScottKnott-package}
\alias{ScottKnott-package}
\alias{ScottKnott}
\docType{package}

\title{
  The ScottKnott Clustering Algorithm}

\description{
  The Scott & Knott clustering algorithm is a very useful clustering algorithm
  widely used as a multiple comparison method in the Analysis of Variance
  context, as for example Gates and Bilbro (1978), Bony et al. (2001),
  Dilson et al. (2002) and Jyotsna et al. (2003).

  It was developed by Scott, A.J. and Knott, M. (Scott and Knott, 1974).
  All methods used up to that date as, for example, the t-test, Tukey, Duncan,
  Newman-Keuls procedures, have overlapping problems. By overlapping we mean
  the possibility of one or more treatments to be classified in more than one
  group, in fact, as the number of treatments reach a number of twenty or more,
  the number of overlappings could increse as reaching 5 or greater what makes
  almost impossible to the experimenter to really distinguish the real groups
  to which the means should belong. The Scott & Knott method does not
  have this problem, what is often cited as a very good quality of this
  procedure.

  The Scott & Knott method make use of a clever algorithm of cluster analysis,
  where, starting from the the whole group of observed mean effects, it divides,
  and keep dividing the sub-groups in such a way that the intersection of any
  two groups formed in that manner is empty.

  Using their own words `we study the consequences of using a well-known method
  of cluster analysis to partition the sample treatment means in a balanced
  design and show how a corresponding likelihood ratio test gives a method of
  judging the significance of difference among groups abtained'.

  Many studies, using the method of Monte Carlo, suggest that the Scott Knott
  method performs very well compared to other methods due to fact that it has
  high power and type I error rate almost always in accordance with the nominal
  levels. The ScottKnott package performs this algorithm starting either from
  \code{vectors}, \code{matrices} or \code{data.frames} joined as
  \code{default}, a \code{aov} or \code{aovlist} resulting object of previous
  analysis of variance. The results are given in the usual way as well as in
  graphical way using thermometers with diferent group colors.

  In a few words, the test of Scott & Knott is a clustering algorithm used as an
  one of the alternatives where multiple comparizon procedures are applied with
  a very important and almost unique characteristic: it does not present
  overlapping in the results.}

\author{
  Enio Jelihovschi (\email{eniojelihovs@gmail.com})\cr
  Jose Claudio Faria (\email{joseclaudio.faria@gmail.com})\cr
  Ivan Bezerra Allaman (\email{ivanalaman@gmail.com})\cr}

\references{
  Bony S., Pichon N., Ravel C., Durix A., Balfourier F., Guillaumin J.J. 2001. The
  Relationship be-tween Mycotoxin Synthesis and Isolate Morphology in Fungal
  Endophytes of Lolium perenne. \emph{New Phytologist}, \bold{1521}, 125-137.

  Borges L.C., FERREIRA D.F. 2003. Poder e taxas de erro tipo I dos testes
  Scott-Knott, Tukey e Student-Newman-Keuls sob distribuicoes normal e nao
  normais dos residuos. Power and type I errors rate of Scott-Knott, Tukey and
  Student-Newman-Keuls tests under normal and no-normal distributions of the
  residues. \emph{Rev. Mat. Estat.}, Sao Paulo, \bold{211}: 67-83.

  Calinski T., Corsten L.C.A. 1985. Clustering Means in ANOVA by Simultaneous
  Testing. \emph{Bio-metrics}, \bold{411}, 39-48.

  Da Silva E.C, Ferreira D.F, Bearzoti E. 1999. Evaluation of power and type I
  error rates of Scott-Knotts test by the method of Monte Carlo.
  \emph{Cienc. agrotec.}, Lavras, \bold{23}, 687-696.

  Dilson A.B, David S.D., Kazimierz J., William W.K. 2002. Half-sib progeny evaluation
  and selection of potatoes resistant to the US8 genotype of Phytophthora
  infestans from crosses between resistant and susceptible parents.
  \emph{Euphytica}, \bold{125}, 129-138.

  Gates C.E., Bilbro J.D. 1978. Illustration of a Cluster Analysis Method for Mean
  Separation. \emph{Agron J}, \bold{70}, 462-465.

  Wilkinson, G.N, Rogers, C.E. 1973. Journal of the Royal Statistical Society.
  Series C (Applied Statistics), Vol. 22, No. 3, pp. 392-399.

  Jyotsna S., Zettler L.W., van Sambeek J.W., Ellersieck M.R., Starbuck C.J. 2003.
  Symbiotic Seed Germination and Mycorrhizae of Federally Threatened Platanthera
  PraeclaraOrchidaceae.
  \emph{American Midland Naturalist}, \bold{1491}, 104-120.

  Ramalho M.A.P., Ferreira DF, Oliveira AC 2000. \emph{Experimentacao em Genetica
    e Melhoramento de Plantas}. Editora UFLA.

  Scott R.J., Knott M. 1974. A cluster analysis method for grouping mans in the
  analysis of variance. \emph{Biometrics}, \bold{30}, 507-512.}

\keyword{package}
\keyword{htest}
\keyword{univar}
\keyword{tree}
\keyword{design}

\examples{
##
## Examples: Completely Randomized Design (CRD)
## More details: demo(package='ScottKnott')
##

## The parameters can be: vectors, design matrix and the response variable,
## data.frame or aov
data(CRD2)

## From: design matrix (dm) and response variable (y)
sk1 <- with(CRD2,
            SK(x=dm,
               y=y,
               model='y ~ x',
               which='x'))
summary(sk1)
plot(sk1,
     col=rainbow(max(sk1$groups)),
     mm.lty=3, 
     id.las=2,
     rl=FALSE,
     title='Factor levels')

## From: data.frame (dfm)
sk2 <- with(CRD2, 
            SK(x=dfm,
               model='y ~ x',
               which='x'))
summary(sk2)
plot(sk2,
     col=rainbow(max(sk2$groups)),
     id.las=2,
     rl=FALSE)

## From: aov
av <- with(CRD2,
           aov(y ~ x,
               data=dfm))
summary(av)

sk3 <- with(CRD2,
            SK(x=av,
               which='x'))
summary(sk3)
plot(sk3,
     col=rainbow(max(sk3$groups)),
     rl=FALSE,
     id.las=2,
     title=NULL)

##
## Example: Randomized Complete Block Design (RCBD)
## More details: demo(package='ScottKnott')
##

## The parameters can be: design matrix and the response variable,
## data.frame or aov

data(RCBD)

## Design matrix (dm) and response variable (y)
sk1 <- with(RCBD,
            SK(x=dm,
               y=y,
               model='y ~ blk + tra',
               which='tra'))
summary(sk1)
plot(sk1)

## From: data.frame (dfm), which='tra'
sk2 <- with(RCBD,
            SK(x=dfm,
               model='y ~ blk + tra',
               which='tra'))
summary(sk2)
plot(sk2,
     mm.lty=3,
     title='Factor levels')

##
## Example: Latin Squares Design (LSD)
## More details: demo(package='ScottKnott')
##

## The parameters can be: design matrix and the response variable,
## data.frame or aov

data(LSD)

## From: design matrix (dm) and response variable (y)
sk1 <- with(LSD,
            SK(x=dm,
               y=y,
               model='y ~ rows + cols + tra',
               which='tra'))
summary(sk1)
plot(sk1)

## From: data.frame
sk2 <- with(LSD,
            SK(x=dfm,
               model='y ~ rows + cols + tra',
               which='tra'))
summary(sk2)
plot(sk2,
     title='Factor levels')

## From: aov
av <- with(LSD,
           aov(y ~ rows + cols + tra,
               data=dfm))
summary(av)

sk3 <- SK(av,
          which='tra')
summary(sk3)
plot(sk3,
     title='Factor levels')

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
sk1 <- with(FE,
            SK(x=dm,
               y=y,
               model='y ~ blk + N*P*K',
               which='N'))
summary(sk1)
plot(sk1,
     title='Main effect: N')

## Nested: p1/N
## Studing N inside of level one of P
nsk1 <- with(FE,
             SK.nest(x=dm,
                     y=y,
                     model='y ~ blk + N*P*K',
                     which='P:N',
                     fl1=1))
summary(nsk1)
plot(nsk1,
     title='Effect: p1/N')

## Nested: k1/P
nsk2 <- with(FE,
             SK.nest(x=dm,
                     y=y,
                     model='y ~ blk + N*P*K',
                     which='K:P',
                     fl1=1))
summary(nsk2)
plot(nsk2,
     title='Effect: k1/P')

## Nested: k2/p2/N
nsk3 <- with(FE,
             SK.nest(x=dm,
                     y=y,
                     model='y ~ blk + N*P*K',
                     which='K:P:N',
                     fl1=2,
                     fl2=2))
summary(nsk3)
plot(nsk3,
     title='Effect: k2/p2/N')

## Nested: k1/n1/P
## Studing P inside of level one of K and level one of N
nsk4 <- with(FE, 
             SK.nest(x=dm,
                     y=y,
                     model='y ~ blk + N*P*K',
                     which='K:N:P',
                     fl1=1,
                     fl2=1))
summary(nsk4)
plot(nsk4,
     title='Effect: k1/n1/P')

## Nested: p1/n1/K
nsk5 <- with(FE,
             SK.nest(x=dm,
                     y=y,
                     model='y ~ blk + N*P*K',
                     which='P:N:K',
                     fl1=1,
                     fl2=1))
summary(nsk5)
plot(nsk5, title='Effect: p1/n1/K')

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
sk1 <- with(SPE, 
            SK(x=dm, 
               y=y, 
               model='y ~ blk + P*SP + Error(blk/P)',
               which='P',
               error='blk:P'))
summary(sk1)
plot(sk1)

## Main factor: SP
sk2 <- with(SPE,
            SK(x=dm,
               y=y,
               model='y ~ blk + P*SP + Error(blk/P)',
               which='SP', 
               error='Within'))
summary(sk2)
plot(sk2,
     xlab='Groups',
     ylab='Main effect: sp',
     title='Main effect: SP')

## Nested: p1/SP
skn1 <- with(SPE,
             SK.nest(x=dm,
                     y=y,
                     model='y ~ blk + P*SP + Error(blk/P)',
                     which='P:SP',
                     error='Within',
                     fl1=1 ))
summary(skn1)
plot(skn1,
     title='Effect: p1/SP')

##
## Example: Split-split-plot Experiment (SSPE)
## More details: demo(package='ScottKnott')
##

## Note: The factors are in uppercase and its levels in lowercase!

data(SSPE)

## From: design matrix (dm) and response variable (y)
## Main factor: P
sk1 <- with(SSPE,
            SK(dm,
               y,
               model='y ~ blk + P*SP*SSP + Error(blk/P/SP)',
               which='P',
               error='blk:P'))
summary(sk1)

# Main factor: SP
sk2 <- with(SSPE,
            SK(dm,
               y,
               model='y ~ blk + P*SP*SSP + Error(blk/P/SP)',
               which='SP',
               error='blk:P:SP'))
summary(sk2)

# Main factor: SSP
sk3 <- with(SSPE, 
            SK(dm,
               y,
               model='y ~ blk + P*SP*SSP + Error(blk/P/SP)',
               which='SSP',
               error='Within'))
summary(sk3)

## Nested: p1/SP
skn1 <- with(SSPE, 
             SK.nest(dm,
                     y,
                     model='y ~ blk + P*SP*SSP + Error(blk/P/SP)',
                     which='P:SP',
                     error='blk:P:SP',
                     fl1=1))
summary(skn1)

## From: aovlist
av <- with(SSPE,
           aov(y ~  blk + P*SP*SSP + Error(blk/P/SP),
               data=dfm))
summary(av)   

## Nested: p/sp/SSP (at various levels of sp and p) 
skn6 <- SK.nest(av,
                which='P:SP:SSP',
                error='Within',
                fl1=1,
                fl2=1)
summary(skn6)

skn7 <- SK.nest(av,
                which='P:SP:SSP',
                error='Within',
                fl1=2,
                fl2=1)
summary(skn7)
}
