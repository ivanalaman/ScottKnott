\name{ScottKnott-package}
\alias{ScottKnott-package}
\alias{ScottKnott}
\docType{package}

\title{
 The ScottKnott Clustering Algorithm
}

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
 levels. The \pkg{ScottKnott} package performs this algorithm starting either from
 \code{vectors}, \code{matrices} or \code{data.frames} joined as
 \code{default}, a \code{aov}, \code{aovlist}, \code{lm} and \code{lmer} resulting object of previous
 analysis of variance. The results are given in the usual way as well as in
 graphical way using thermometers with diferent group colors.

 In a few words, the test of Scott & Knott is a clustering algorithm used as an
 one of the alternatives where multiple comparizon procedures are applied with
 a very important and almost unique characteristic: it does not present
 overlapping in the results.

 As of version 1.2-8, the ScottKnott package is able to analyze unbalanced data based on the article `Adjusting the Scott-Knott cluster analyzes for unbalanced designs' by Conrado et al. 

}

\author{
 Enio Jelihovschi (\email{eniojelihovs@gmail.com})\cr
 Jose Claudio Faria (\email{joseclaudio.faria@gmail.com})\cr
 Ivan Bezerra Allaman (\email{ivanalaman@gmail.com})\cr
}

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
 analysis of variance. \emph{Biometrics}, \bold{30}, 507-512.

 Conrado, T. V., Ferreira, D. F., Scapim, C. A., and Maluf, W. R. "Adjusting the Scott-Knott cluster analyses for unbalanced designs." \emph{Crop Breeding and Applied Biotechnology} \bold{17}.1 (2017): 1-9.
}

\keyword{package}
\keyword{htest}
\keyword{univar}
\keyword{tree}
\keyword{design}

