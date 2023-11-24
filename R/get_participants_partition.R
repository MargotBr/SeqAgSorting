get_participants_partition <- function(
  dta.final,
  sep.part.step
  ){

  nbrater <- ncol(dta.final)

  sim.mat.part <- as.data.frame(matrix(NA, nbrater, nbrater))

  rownames(sim.mat.part) <- colnames(sim.mat.part) <- gsub(paste0(sep.part.step, ".*$"), "", colnames(dta.final))

  for (i in 1 : nbrater) {
    for (j in 1 : nbrater) {
      if (is.na(sim.mat.part[i, j])) {
        sim.mat.part[i, j] <- RandIndex(dta.final[,i], dta.final[,j])$AdjustedRand
      }
    }
  }

  dissim.mat.part <- as.dist(1 - sim.mat.part)

  dendrogram <- stats::hclust(dissim.mat.part, method = "ward.D2")

  sink(file = "undesired message")

  nb.clust.part <- invisible(
    NbClust(
      diss = dissim.mat.part,
      distance = NULL,
      method = "ward.D2",
      index = "mcclain",
      min.nc = 2, max.nc = 6)$Best.nc[1]
  )
  sink()

  file.remove("undesired message")

  partition <- cutree(dendrogram, k = nb.clust.part)

  mat.partition <- cbind.data.frame(partition, names(partition))

  colnames(mat.partition) <- c("Cluster", "Rater")

  return(
    list(
      sim.mat.part = sim.mat.part,
      partition = partition,
      mat.partition = mat.partition,
      dendrogram = dendrogram
    )
  )

}
