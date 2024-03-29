\name{panellipse}
\alias{panellipse}

\title{Confidence ellipses around products based on panelists descriptions}

\description{Virtual panels are generated using Boostrap techniques in order to display
confidence ellipses around products.}

\usage{
panellipse(donnee, col.p, col.j, firstvar, lastvar = ncol(donnee),
    alpha = 0.05, coord = c(1,2), scale.unit = TRUE, nbsimul = 500,
    nbchoix = NULL, group = NULL, name.group = NULL,
    level.search.desc = 0.2, centerbypanelist = TRUE,
    scalebypanelist = FALSE, name.panelist = FALSE,
    variability.variable = TRUE, cex = 1, color = NULL)
}

\arguments{
  \item{donnee}{a data frame made up of at least two qualitative variables 
  (\emph{product}, \emph{panelist}) and a set of quantitative variables (sensory descriptors)}  
  \item{col.p}{the position of the \emph{product} variable}
  \item{col.j}{the position of the \emph{panelist} variable}
  \item{firstvar}{the position of the first sensory descriptor}
  \item{lastvar}{the position of the last sensory descriptor (by default the last column of \code{donnee})}
  \item{alpha}{the confidence level of the ellipses}
  \item{coord}{a length 2 vector specifying the components to plot}
  \item{scale.unit}{boolean, if T the descriptors are scaled to unit variance}
  \item{nbsimul}{the number of simulations (corresponding to the number of virtual panels) used to compute the ellipses}
  \item{nbchoix}{the number of panelists forming a virtual panel, by default the number of panelists in the original panel}
  \item{group}{the number of variables in each group of variables when multiple factor analysis is performed (by default this parameter equals NULL and a PCA is performed)}
  \item{name.group}{the names of the groups of variables when mfa is performed (if \code{group} differs from NULL)}  
  \item{level.search.desc}{the threshold above which a descriptor is not considered as discriminant according to AOV model \code{"descriptor=Product+Panelist"}}
  \item{centerbypanelist}{boolean, if T center the data by panelist before the construction of the axes}
  \item{scalebypanelist}{boolean, if T scale the data by panelist before the construction of the axes (by default, FALSE
  is assigned to that parameter)}
  \item{name.panelist}{boolean, if T then the name of each panelist is displayed on the \code{plotpanelist} graph (by default, FALSE
  is assigned to that parameter)}
  \item{variability.variable}{boolean, if T a plot with the variability of the variable is drawn and a confidence intervals of the correlations
  between descriptors are calculated}
  \item{cex}{cf. function \code{\link{par}} in the \pkg{graphics} package}
 \item{color}{a vector with the colors used; by default there are 35 colors defined}
}


\details{
Panellipse, step by step:\cr
Step 1 Performs a selection of discriminating descriptors with respect to a threshold set by users \cr
Step 2 Virtual panels are generated using Boostrap techniques; the number of panels as well as their size
are set by users with the \emph{nbsimul} and \emph{nbchoix} parameters \cr
Step 3 Coordinates of the products with respect to each virtual panels are computed \cr
Step 4 Each product is then circled by its confidence ellipse generated by virtual panels and
comprising (1-alpha)*100 percent of the virtual products \cr
Step 5 Variability of the variables is drawn and confidence interval of the correlation coefficient between descriptors are calculated by bootstrap \cr
}
\value{
A list containing the following elements:
  \item{eig}{a matrix with the component of the factor analysis (in row) and the eigenvalues, the inertia and the cumulative inertia for each component}
  \item{coordinates}{a list with: the coordinates of the products with respect to the panel and to each panelists
 and the coordinates of the \emph{partial} products with respect to the panel and to each panelists}
  \item{hotelling}{Returns a matrix with the P-values of the Hotelling's T2 tests for each pair of products: this matrix allows to find the
product which are significantly different for the 2-components sensory description; if an MFA is done, \emph{hotelling} returns as many matrices 
as there are group, these matrices allows to find the product which are significantly different for the 2-components sensory description of the
group, and it returns also a \emph{global} matrix corresponding to the P-values for the tests corresponding to the mean product.}
\item{correl}{a list with: the matrix of the estimated correlation coefficients and two matrices corresponding to the confidence intervals, min and max, of the correlation coefficients calculated by bootstrap.}

Returns a graph of the products as well as a correlation circle of the descriptors.\cr

Returns a graph where each product is displayed with respect to a panel and to each panelist composing
the panel; products described by the panel are displayed as square, they are displayed as circle when
they are described by each panelist.\cr

Returns a graph where each product is circled by its confidence ellipse generated by virtual panels.
When a Multiple Factor Analysis is performed, returns a graph where each partial product is circled by its confidence ellipse generated by virtual panels.\cr

Returns a graph where the variability of each variable is drawn on the correlation circle graph. \cr
}

\seealso{ \code{\link{panellipse.session}}, \code{\link{panelmatch}}}

\references{
Husson F., Le Dien S. & Pag�s J.  (2005). Confidence ellipse for the sensory profiles obtained by Principal Components Analysis. \emph{Food Quality and Preference}.  16 (3), 245-250. \cr
Pag�s J. & Husson F. (2005). Multiple Factor Analysis with confidence ellipses: a methodology to study the relationships between sensory and instrumental data. To be published in \emph{Journal of Chemometrics}. \cr
Husson F., L� S. & Pag�s J.  Variability of the representation of the variables resulting from PCA in the case of a conventional sensory profile. \emph{Food Quality and Preference}.  16 (3), 245-250. 
}
\author{Fran�ois Husson}

\examples{
\dontrun{
## Example 1: PCA
data(chocolates)
res <- panellipse(sensochoc, col.p = 4, col.j = 1, firstvar = 5)
coltable(res$hotelling, main.title = "P-values for the Hotelling's T2 tests")

## If we consider only 12 panelists in a virtual panel, 
## what would be the size of the ellipses
res2 <- panellipse(sensochoc, col.p = 4, col.j = 1, nbchoix = 12, firstvar = 5)
coltable(res2$hotelling, main.title = "P-values for the Hotelling's T2 tests")

## If we want the confidence ellipses around the individual descriptions
panellipse(sensochoc, col.p = 4, col.j = 1, nbchoix = 1, firstvar = 5)


## Example 2: MFA
data(chocolates)
res <- panellipse(sensochoc, col.p = 4, col.j = 1, firstvar = 5, 
    group = c(6,8), name.group = c("G1","G2"))
for (i in 1:dim(res$hotelling$bygroup)[3]) coltable(res$hotelling$bygroup[,,i], 
    main.title = paste("P-values for the Hotelling's T2 tests (",
    dimnames(res$hotelling$bygroup)[3][[1]][i],")",sep=""))
}
}
\keyword{multivariate}
