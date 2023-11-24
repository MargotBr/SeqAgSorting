get_stimuli_map <- function(
  dta.final,
  axis
  ) {

  res.mca <- MCA(dta.final, graph = FALSE)
  coord.stim <- as.data.frame(res.mca$ind$coord[, axis])
  coord.stim <- as.data.frame(res.mca$ind$coord[, axis])

  res.hcpc <- HCPC(res.mca, nb.clust = -1, graph = FALSE)
  vec.clust <- as.data.frame(res.hcpc$data.clust$clust)
  rownames(vec.clust) <- rownames(res.hcpc$data.clust)
  colnames(vec.clust) <- "Cluster"

  coord.stim <- merge(coord.stim, vec.clust, by = "row.names")
  rownames(coord.stim) <- coord.stim[, 1]
  coord.stim <- coord.stim[, -1]
  colnames(coord.stim)[1 : 2] <- c("AxeA", "AxeB")

  return(
    list(
      res.mca = res.mca,
      coord.stim = coord.stim
    )
  )

}
