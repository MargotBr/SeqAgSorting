\name{SAStask-package}
\alias{SAStask-package}
\alias{SAStask}
\docType{package}
\title{
\packageTitle{SAStask}
}
\description{
\packageDescription{SAStask}
}

\author{
Margot Brard, Sebastien Le

Maintainer: Margot Brard \email{margot.brard@agrocampus-ouest.fr}
}

\keyword{ package }
\seealso{
\code{\link{AnalyseSAS}}
\code{\link{plot.AnalyseSAS}}
}
\examples{
# Small pedagogic data set
data(pedagdata)
res.pedag <- AnalyseSAS(dta = pedagdata, sast.parameters = c(4, 3, 3), id.info.stim = 31, type.info.stim = "cat", id.info.part = 11 : 12, type.info.part = rep("cat", 2), nbtimes.consens.charact = 2, proba.consens.charact = 0.10, graph = FALSE)
res.pedag
plot.AnalyseSAS(res.pedag, choice = "stim", interact = TRUE)
plot.AnalyseSAS(res.pedag, choice = "part", interact = TRUE, col.clust.part = c("orange", "green"))
}
