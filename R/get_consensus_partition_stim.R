get_consensus_partition_stim <- function(
  partition,
  sim.mat.part,
  sep.charact,
  dta.final
  ) {

  consensus.partition.stim <- list()
  nbstim <- nrow(dta.final)

  for (i in 1 : nlevels(as.factor(partition))) {

    consensus.partition.stim[[i]] <- list()
    dta.clust.sauv <- dta.final[, which(colnames(sim.mat.part) %in% names(which(partition == i)))]
    dta.clust <- SortingPartition(dta.clust.sauv)
    consens.part <- ConsensusPartition(dta.clust)$Consensus
    consensus.partition.stim[[i]][[1]] <- consens.part
    dta.charact.by.stim.clust <- as.data.frame(matrix(NA, nrow(dta.final), 1))
    rownames(dta.charact.by.stim.clust) <- rownames(dta.final)
    colnames(dta.charact.by.stim.clust) <- "Description"

    for (j in 1 : nrow(dta.charact.by.stim.clust)) {
      dta.charact.by.stim.clust[j, 1] <- paste(unlist(strsplit(dta.clust.sauv[j,], sep.charact)), collapse = " ")
    }

    consens.part <- as.data.frame(consens.part)
    colnames(consens.part) <- "Cluster"
    dta.charact.by.stim.clust <- merge(consens.part, dta.charact.by.stim.clust, by = "row.names")
    rownames(dta.charact.by.stim.clust) <- dta.charact.by.stim.clust[, 1]
    dta.charact.by.stim.clust <- dta.charact.by.stim.clust[, -1]
    res.textual.clust <- textual(dta.charact.by.stim.clust, num.text = 2, contingence.by = 1)
    res.desc.clust.stim.clust.text <- descfreq(res.textual.clust$cont.table)
    names(res.desc.clust.stim.clust.text) <- paste0("category.stim", names(res.desc.clust.stim.clust.text))
    consensus.partition.stim[[i]][[2]] <- res.desc.clust.stim.clust.text
    names(consensus.partition.stim[[i]]) <- c("partition", "characterisation")

  }

  names(consensus.partition.stim) <- paste0("category", 1 : length(consensus.partition.stim))

  return(consensus.partition.stim)

}
