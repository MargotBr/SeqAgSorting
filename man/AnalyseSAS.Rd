\name{AnalyseSAS}
\alias{AnalyseSAS}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Statistical analysis of Sequential Agglomerative Sorting (SAS) task data}
\description{Performs the statistical analysis of SAS data through a stimulus-oriented approach and a participant-oriented approach. The method is able to manage covariates providing information about the participants and the stimuli to facilitate the interpretation of the results.}
\usage{
AnalyseSAS(dta, sast.parameters, sep.part.step = "_", sep.charact = " ; ", nbtimes.consens.charact = 5, proba.consens.charact = 0.05, id.info.stim = NULL, type.info.stim = NULL, id.info.part = NULL, type.info.part = NULL, graph = TRUE, ext.dev.Rstudio = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dta}{
A data frame of dimensions S x (P * Nstep) (S=number of stimuli, P=number of participants, Nstep=number of steps during the SAS task). This is the minimum needed. But dta can also contain covariates about the stimuli as extra columns and covariates about the participants as extra lines. See the pedagdata dataset for an example.}
  \item{sast.parameters}{
A vector of integer elements specifying the number of stimuli in each subset during the SAS task.}
  \item{sep.part.step}{
A character element corresponding to the separator character between the names of the participants and the steps of the SAS task in the columns names of dta. By default, this argument is fixed to "_", meaning that the columns names are in the following form: "part1_1", "part1_2", etc.}
  \item{sep.charact}{
A character element corresponding to the characteristics separator character in dta. By default, this argument is fixed to " ; ", meaning that two characteristics are separated by " ; " in dta.}
  \item{nbtimes.consens.charact}{
An integer element corresponding to the minimum number of citations needed to include a characteristic in the textual analysis. This argument is optional and, by default, it is fixed to 5, meaning that the characteristics used less than 5 times at a panel levels are excluded from the textual analysis.}
  \item{proba.consens.charact}{
An integer element corresponding to the significance threshold considered to consider a characteristic as consensual during the textual analysis. This argument is optional and, by default, it is fixed to 0.05.}
 \item{id.info.stim}{
A vector of integer elements composed of the identification of the columns containing the supplementary information (i.e. covariates) about the stimuli. This argument is optional and, by default, it is fixed to NULL, meaning that dta does not contain supplementary information about the stimuli.}
  \item{type.info.stim}{
A vector of character elements composed of the type of the covariates about the stimuli. This vector must be of the same length that id.info.stim. A continuous covariate is associated to 'cont' and a categorical covariate is associated to 'cat'. This argument is optional and, by default, it is fixed to NULL, meaning that dta does not contain supplementary information about the stimuli.}
  \item{id.info.part}{
A vector of integer elements composed of the identification of the lines containing the supplementary information (i.e. covariates) about the participants. This argument is optional and, by default, it is fixed to NULL, meaning that dta does not contain supplementary information about the participants.}
  \item{type.info.part}{
A vector of character elements composed of the type of the covariates about the participants. This vector must be of the same length that id.info.part. A continuous covariate is associated to 'cont' and a categorical covariate is associated to 'cat'. This argument is optional and, by default, it is fixed to NULL, meaning that dta does not contain supplementary information about the participants.}
  \item{graph}{
A boolean specifying if the graphical outputs should be plotted or not. By default, they are plotted.}
  \item{ext.dev.Rstudio}{
A boolean specifying if the graphical outputs should be plotted in the Rstudio plot pane or not.}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
\item{res.mca}{All the results of the MCA realized during the stimulus-oriented statistical analysis. MCA is only performed on the data provided at the last step of the SAS task.}
\item{coocc.stim}{Cooccurence matrix between stimuli (similarity matrix). Provides the number of times that each pair of stimuli appears in a same group at a panel level at the last step of the SAS task.}
\item{partition.stim}{Clusters of stimuli. They are extracted from a dissimilarity matrix based on their coordinates on the MCA components.}
\item{remark.stim}{Remarkability of the stimuli during the SAS task. Indicates, for instance, if a stimulus has been perceived as insignificant during the first steps but remarkable during the last step.}
\item{consensual.charact.stim}{Characteristics used consensually to describe the stimuli at the last step of the SAS task. These characteristics are plotted on the stimuli representation.}
\item{charact.clust.stim}{Automatic characterization of the clusters of stimuli.}
\item{ARI.part}{Similarity matrix between participants. The similarity index corresponds to the Adjusted Rand Index (ARI) between the partitions of the stimuli provided by the two corresponding participants.}
\item{partition.part}{Clusters of participants. They are extracted from the dissimilarity matrix based on 1-ARI.}
\item{cons.partition.stim.clust}{Consensus (or Average) partition of the stimuli in each cluster of participants.}
\item{charact.clust.part}{Automatic characterization of the clusters of participants.}
}

\references{
%% ~put references to the literature/web site here ~
}
\author{Margot Brard \email{margot.brard@agrocampus-ouest.fr}}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{plot.AnalyseSAS}}
}
\examples{
# Small pedagogic data set
data(pedagdata)
res.pedag <- AnalyseSAS(dta = pedagdata, sast.parameters = c(4, 3, 3), id.info.stim = 31, type.info.stim = "cat", id.info.part = 11 : 12, type.info.part = rep("cat", 2), nbtimes.consens.charact = 2, proba.consens.charact = 0.10, graph = FALSE)
res.pedag

# Perfumes data set
res.perfumes <- AnalyseSAS(dta = perfumes, sast.parameters = c(12, 14, 14), sep.charact = " ", id.info.part = 41 : 42, type.info.part = rep("cont", 2))
res.perfumes

# Lemon tart recipes data set
data(lemontart)
res.lemontart <- AnalyseSAS(dta = lemontart, sast.parameters = c(9, 9, 9), sep.charact = " ", id.info.stim = 199 : 201, type.info.stim = rep("cont", 3), id.info.part = 28 : 30, type.info.part = rep("cont", 3))
res.lemontart
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
