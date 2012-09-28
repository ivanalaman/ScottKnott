\name{MaxValue}
\alias{MaxValue}

\title{
  Algorithm for Pre-order Path in Binary Decision Tree}

\description{
  Builds groups of means, according to the method of Scott & Knott.}

\usage{
  MaxValue(g,
           means,
           mMSE,
           dfr, 
           sig.level,
           k,
           group,
           ngroup,
           markg,
           g1=g,
           sqsum=rep(0, g1))}

\arguments{
  \item{g}{A vector of length 1 giving the upper limit of the possible groups.}
  \item{means}{A vector of the group of treatments means in decresing order.}
  \item{mMSE}{A vector of length 1 giving the MSE divided by the number of replications.}
  \item{dfr}{A vector of length 1 giving the degrees of freedom of MSE.}
  \item{sig.level}{A vector of length 1 giving the level of significance of the test.}
  \item{k}{A vector of length 1 giving the lower limit of the possible groups.}
  \item{group}{A vector of the same length as \code{means} marking the groups generated.}
  \item{ngroup}{A vector of length 1 giving the number of groups generated.}
  \item{markg}{A vector of the same length as \code{means} marking the upper limit of the last group generated before the process goes on recursively.}
  \item{g1}{A vector of length 1 which keeps, during the whole process, the value of the initial \code{g}.}
  \item{sqsum}{A vector of length 1 giving the sum of the square between groups.}}

\details{
  The function MaxValue builds groups of means, according to the method of
  Scott & Knott.

  Basically it is an algorithm for pre-order path in binary decision tree.

  Every node of this tree, represents a different group of means and, when the
	algorithm reaches this node it takes the decision to either split the group
	in two, or form a group of means.

  If the decision is to divide then this node generates two children and the
	algorithm follows for the node on the left, if, on the other hand, the
	decision is to form a group, then it returns to the parent node of that node
	and follows to the right node.

  In this way it follows until the last group is formed, the one containing
  the highest (or the least) mean. In case that the highest (or the least)
  mean becomes itself a group of one element, the algorithm continues to
  the former group. In the end, each node without children represents a group
  of means.}

\value{
  An \emph{vector} with the groups of means.}

\note{
	This function is mainly for internal use in the ScottKnott package.}

\author{
  Enio Jelihovschi (\email{eniojelihovs@gmail.com})\cr
  Jose Claudio Faria (\email{joseclaudio.faria@gmail.com})\cr
  Ivan Bezerra Allaman (\email{ivanalaman@gmail.com})\cr}

\references{
  Ramalho M.A.P., Ferreira D.F., Oliveira A.C. 2000. \emph{Experimentacao em Genetica
  e Melhoramento de Plantas}. Editora UFLA.

  Scott R.J., Knott M. 1974. A cluster analysis method for grouping mans in the
  analysis of variance. \emph{Biometrics}, \bold{30}, 507-512.}

\seealso{\code{\link{SK}}}

\examples{
##
## Examples: Completely Randomized Design (CRD)
##

data(CRD1)

av      <- with(CRD1,
                aov(y ~ x,
                    data=dfm))              # Doing an ANOVA
mm      <- model.tables(av,
                        "means")            # summary tables for model fits
tabs    <- mm$tables[-1]                    # all model means
which   <- names(av$model)[2]
tabs    <- tabs[which]                      # specified group means
nn      <- mm$n[names(tabs)]                # repetions number of specified groups
MSE     <- sum(resid(av)^2)/av$df.residual
tab     <- tabs[[which]]                    # tab=means
means   <- as.vector(tab)
mnumber <- length(means)                    # number of means
nms     <- names(tab)
r       <- nn[[which]]                      # groups and its number of replicates
ord     <- order(means,
                 decreasing=TRUE)
mMSE    <- MSE/r
dfr     <- av$df.residual                   # residual degrees of freedom
means   <- means[ord]                       # decreasing ordered means
g       <- mnumber

## Grouping the means: sig.level=.05
groups  <- MaxValue(g,
                    means,
                    mMSE,
                    dfr,
                    sig.level=.05,
                    1,
                    rep(0, g),
                    0,
                    rep(0, g))

## The result: sig.level=.05
groups


## Grouping the means: sig.level=.10
groups  <- MaxValue(g,
                    means,
                    mMSE,
                    dfr,
                    sig.level=.10,
                    1,
                    rep(0, g),
                    0,
                    rep(0, g))

## The result: sig.level=.10
groups
}

\keyword{package}
\keyword{htest}
\keyword{univar}
\keyword{tree}
\keyword{design}

