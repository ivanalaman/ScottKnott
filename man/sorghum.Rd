\name{sorghum}
\alias{sorghum}

\title{
  Completely Randomized Design (CRD)
}

\description{
  The experiment consists of 16 treatments (cultivars) of sorghum conducted in a 
  balanced squared lattice design and the yield by plot (kg/plot).
}

\usage{
  data(sorghum)
  sorghum
}

\format{
  An incomplete balanced block design with 4 blocks, 16 treatments, 
  and 5 repetitions, that is, the yield of each treatment is measured 5 times.
  \code{sorghum} is a list with 4 elements. The first \samp{tr} is a factor of lenght 80
  with 16 levels describing the treatments. The second \samp{dm} is data.frame 
  describing the design matrix. Its columns are \samp{x}, \samp{bl} (blocks) and \samp{r} 
  repetitions. The third \samp{y} is a numeric vector the yields. The fourth \samp{dfm} 
  is a data frame with four columns. The first tree columns are the design matrix
  and the fourth is \samp{y}.
}

\details{
  The experiment was conducted at EMBRAPA Milho e Sorgo 
  (The Brazilian Agricultural Research Corporation, Corn and Sorghum section).
}

\source{
  Ramalho, M.A.P. and Ferreira, D.F. and Oliveira, A.C. (2000).
  Experimentacao em Genetica e Melhoramento de Plantas. Editora UFLA, Lavras, 
  Brazil, page 167.
}

\keyword{datasets}

\examples{
library(ScottKnott)

data(sorghum) 

av <- aov(y ~ r/bl + x,
          data=sorghum$dfm)

sk <- SK(av,
         which='x',
         sig.level=0.05) 

summary(sk)

plot(sk)
}  
