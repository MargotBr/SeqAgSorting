\name{plot.AnalyseSAS}
\alias{plot.AnalyseSAS}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Graphical representation of the results of the statistical analysis of Sequential Agglomerative Sorting (SAS) task data}
\description{Draws the graphs provided by the statistical analysis of SAS task data. Tunning parameters such as the colors or the axis of MCA to be plotted can be specified. The function also allows to plot interactive outputs realized with plotly.}
\usage{
plot.AnalyseSAS(res, choice = "all", interact = FALSE, col.clust.stim = NULL, col.clust.part = NULL, axis = c(1, 2), ext.dev.Rstudio = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{res}{
An object of class AnalyseSAS returned by the function AnalyseSAS.}
  \item{choice}{
A character element specifying the graphs to be plotted ('stim' for the stimuli mapping, 'part' for the representation of the segmentation process of the participants (dendrogram), 'all' for all representations. By default, all representations are plotted.)}
  \item{interact}{
A boolean specifying if the graphical outputs should be interactive (with plotly) or not.}
  \item{col.clust.stim}{
A color element that can be specified by a character such as "blue" or by a hexadecimal code such as "#0D10D5". All hexadecimal codes can be found at \url{http://htmlcolorcodes.com}. By default, no color is specified and default color is used.}
  \item{col.clust.part}{
A color element that can be specified by a character such as "blue" or by a hexadecimal code such as "#0D10D5". All hexadecimal codes can be found at \url{http://htmlcolorcodes.com}. By default, no color is specified and default color is used.}
  \item{axis}{
A length 2 numeric vector specifying the MCA components to be plotted. By default, the first 2 components are plotted.}
\item{ext.dev.Rstudio}{
A boolean specifying if the graphical outputs should be plotted in the Rstudio plot pane or not.}
}
\value{
Returns the graphical representations.
}
\references{
%% ~put references to the literature/web site here ~
}
\author{Margot Brard \email{margot.brard@agrocampus-ouest.fr}}
\seealso{
\code{\link{AnalyseSAS}}
}
\examples{
# Small pedagogic data set
data(pedagdata)
res.pedag <- AnalyseSAS(dta = pedagdata, sast.parameters = c(4, 3, 3), id.info.stim = 31, type.info.stim = "cat", id.info.part = 11 : 12, type.info.part = rep("cat", 2), nbtimes.consens.charact = 2, proba.consens.charact = 0.10, graph = FALSE)
plot.AnalyseSAS(res.pedag, choice = "stim")
plot.AnalyseSAS(res.pedag, choice = "stim", interact = TRUE)
plot.AnalyseSAS(res.pedag, choice = "part", interact = TRUE, col.clust.part = c("orange", "green"))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
