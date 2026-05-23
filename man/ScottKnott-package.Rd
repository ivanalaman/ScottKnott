\name{ScottKnott-package}
\alias{ScottKnott-package}
\alias{ScottKnott}
\docType{package}

\title{
 The ScottKnott Clustering Algorithm
}

\description{
 The Scott & Knott clustering algorithm is a widely used multiple comparison
 method in the Analysis of Variance context (Gates and Bilbro, 1978; Bony
 et al., 2001; Dilson et al., 2002; Jyotsna et al., 2003).

 Proposed by Scott and Knott (1974), the method overcomes the overlapping
 problem common to other procedures such as the t-test, Tukey, Duncan, and
 Newman-Keuls tests. Overlapping occurs when one or more treatments are
 simultaneously assigned to more than one group; as the number of treatments
 grows to twenty or more, this ambiguity can make it virtually impossible for
 the experimenter to distinguish the true group structure. The Scott & Knott
 method does not have this problem, which is widely regarded as one of its
 main advantages.

 The method uses a cluster analysis algorithm that, starting from the complete
 set of observed treatment means, recursively partitions them so that any two
 resulting groups are disjoint.

 In their own words: \dQuote{we study the consequences of using a well-known
 method of cluster analysis to partition the sample treatment means in a
 balanced design and show how a corresponding likelihood ratio test gives a
 method of judging the significance of difference among groups obtained}.

 Monte Carlo studies suggest that the Scott & Knott method has high power and
 type I error rates that closely follow the nominal levels. The
 \pkg{ScottKnott} package applies this algorithm to objects of class
 \code{formula}, \code{aov}, \code{aovlist}, \code{lm}, and \code{lmerMod}
 from a prior analysis of variance, and presents results both numerically
 and graphically.

 As of version 1.2-8, the package handles unbalanced designs using adjusted
 means, as described in Conrado et al. (2017).
}

\author{
 Faria, J. C. (\email{joseclaudio.faria@gmail.com})\cr
 Jelihovschi, E. G. (\email{eniojelihovs@gmail.com})\cr
 Allaman, I. B. (\email{ivanalaman@gmail.com})\cr
}

\references{
 Bony S., Pichon N., Ravel C., Durix A., Balfourier F., Guillaumin J.J. 2001. The
 Relationship between Mycotoxin Synthesis and Isolate Morphology in Fungal
 Endophytes of Lolium perenne. \emph{New Phytologist}, \bold{1521}, 125-137.

 Borges L.C., FERREIRA D.F. 2003. Poder e taxas de erro tipo I dos testes
 Scott-Knott, Tukey e Student-Newman-Keuls sob distribuicoes normal e nao
 normais dos residuos. Power and type I errors rate of Scott-Knott, Tukey and
 Student-Newman-Keuls tests under normal and non-normal distributions of the
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

 Scott R.J., Knott M. 1974. A cluster analysis method for grouping means in the
 analysis of variance. \emph{Biometrics}, \bold{30}, 507-512.

 Conrado, T. V., Ferreira, D. F., Scapim, C. A., and Maluf, W. R. "Adjusting the Scott-Knott cluster analyses for unbalanced designs." \emph{Crop Breeding and Applied Biotechnology} \bold{17}.1 (2017): 1-9.
}

\keyword{package}
\keyword{htest}
\keyword{univar}
\keyword{tree}
\keyword{design}
