get_desc_cluster_participants <- function(
  id.info.part,
  id.info.stim,
  dta.sauv,
  sast.parameters,
  partition,
  sep.part.step,
  type.info.part
  ) {

  if (!is.null(id.info.part)) {
    dta.info.part <- dta.sauv[id.info.part,]
    if (!is.null(id.info.stim)) {
      dta.info.part <- dta.info.part[, -id.info.stim]
    }
    dta.info.part <- t(dta.info.part[, seq(1, ncol(dta.info.part), by = length(sast.parameters))])
    rownames(dta.info.part) <- gsub(paste0(sep.part.step, ".*$"), "", rownames(dta.info.part))
    dta.info.part <- merge(dta.info.part, partition, by = "row.names")
    colnames(dta.info.part)[ncol(dta.info.part)] <- "Cluster"
    dta.info.part$Cluster <- as.factor(dta.info.part$Cluster)
    rownames(dta.info.part) <- dta.info.part[, 1]
    dta.info.part <- dta.info.part[, -1]
    for (i in 1 : length(type.info.part)) {
      if (type.info.part[i] == "cat") {
        dta.info.part[, i] <- as.factor(dta.info.part[, i])
      }
    }
    res.desc.clust.part <- catdes(dta.info.part, num.var = which(colnames(dta.info.part) == "Cluster"))
    desc_clusters_participants <- res.desc.clust.part
  } else {
    desc_clusters_participants <- list(NULL)
  }

  return(desc_clusters_participants)

}
