\name{boot}

\alias{boot}

\title{Simulate virtual panels for several functions}

\description{Simulate virtual panels for the sorting task, the napping, the sorting napping, 
the free choice profiling, the hierarchical sorting task}

\usage{
boot(X, method = "sorting", axes = 1:2, scale = TRUE, ncp = max(axes), group = NULL,
    nbsim = 200,level.conf = 0.95,nbchoix = NULL,color = NULL,cex = 0.8, title = NULL){
}

\arguments{
  \item{X}{data.frame}
  \item{method}{String with the method to use. The argument can be "sorting" (the default for sorting task data), 
"napping" (for napping data), "sortnapping" (for sorted napping), 
"freechoice" (for free choice profiling), "hsort" (for hierarchical sorting task data).}  
  \item{axes}{a length 2 vector specifying the components to plot}
  \item{scale}{boolean, used when method="freechoice"; if TRUE, the variables are scaled}
  \item{ncp}{number of components used to procrustes the virtual subspaces on the true subspace}
  \item{group}{a list indicating the number of variables in each group; used when method="freechoice" or method="hsort"}
  \item{nbsim}{the number of simulations (corresponding to the number of virtual panels) used to compute the ellipses}
  \item{level.conf}{confidence level used to construct the ellipses. By default, 0.95}
  \item{nbchoix}{the number of panelists forming a virtual panel, by default the number of panelists in the original panel}
  \item{color}{a vector with the colors used; by default there are 35 colors defined}
  \item{cex}{cf. function \code{\link{par}} in the \pkg{graphics} package}
  \item{title}{string corresponding to the title of the graph you draw (by default NULL and a title is chosen)}
  \item{new.plot}{boolean, if TRUE, a new graphical device is created}
}

\details{
Calculate virtual panels by bootstrap of the panelists. For each virtual panel, calculate the mean 
configuration and procrustes this configuration on the true configuration obtained from the true panel.
}

\references{Husson, F., Le Dien, S. & Pagès, J.  (2005). Confidence ellipse for the sensory profiles obtained by Principal Components Analysis. Food Quality and Preference.  16 (3). pp. 245-250.
}

\author{Marine Cadoret and François Husson}

\examples{
######## Napping example
data(napping)
res <- boot(napping.don,method="napping")

######## Sorted task napping example
data(smoothies)
indice <- (2:73)-rep(c(0,0,3),24)
smoothies <- smoothies[,indice]
boot(smoothies,method="sortnapping")

######## Sorting task example
data(perfume)
res <- boot(perfume,method="sorting",ncp=3)
}

\keyword{multivariate}
\keyword{dplot}
