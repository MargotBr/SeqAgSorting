print.AnalyseSAS <- function (res){

  if (!inherits(res, "AnalyseSAS")){
    stop("Non convenient data - res should be an AnalyseSAS object")
  }

  cat("** Results for the analysis of Sequentital Agglomerative Sorting task (SAStask) data **\n")
  cat("\n")
  cat("The analysis was performed on", (nrow(res$call$dta) - length(res$call$id.info.part)),
      "stimuli assessed by", ((ncol(res$call$dta) - length(res$call$id.info.stim))) / length(res$call$sast.parameters), "participants\n")
  cat("The results are available in the following objects:\n\n")
  res.desc <- array("", c(11, 2), list(1 : 11, c("name", "description")))
  res.desc[1, ] <- c("$call", "arguments used in the AnalyseSAS function")
  res.desc[2, ] <- c("$res.mca", "MCA results")
  res.desc[3, ] <- c("$coocc.stim", "cooccurence matrix (similarity matrix between stimuli)")
  res.desc[4, ] <- c("$partition.stim", "clusters of stimuli")
  res.desc[5, ] <- c("$emark.stim", "remarkability of the stimuli during the SAS task")
  res.desc[6, ] <- c("$consensual.charact.stim", "characteristics used consensually to describe the stimuli")
  res.desc[7, ] <- c("$charact.clust.stim", "automatic description of the clusters of stimuli")
  res.desc[8, ] <- c("$ARI.part", "Adjusted Rand Index matrix (similarity matrix between participants)")
  res.desc[9, ] <- c("$partition.part", "clusters of participants")
  res.desc[10, ] <- c("$cons.partition.stim.clust", "consensus partition of the simuli per cluster of participants")
  res.desc[11, ] <- c("$charact.clust.part", "automatic description of the clusters of participants")
  print(res.desc)

}
