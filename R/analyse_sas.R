analyse_sas <- function(
    dta,
    sast.parameters,
    sep.part.step = "_",
    sep.charact = " ; ",
    nbtimes.consens.charact = 5,
    proba.consens.charact = 0.05,
    id.info.stim = NULL,
    type.info.stim = NULL,
    id.info.part = NULL,
    type.info.part = NULL,
    axis = c(1, 2),
    graph = TRUE,
    ext.dev.Rstudio = FALSE
  ) {

  cli::cli_process_start(
    "Analysing Sequential Agglomerative Sorting task data"
  )

  # Save the initial data set
  cli::cli_bullets(
    "Save the initial data set"
  )
  dta <- as.data.frame(dta)
  dta.sauv <- dta

  # Remove external information about raters and stimuli
  cli::cli_bullets(
    "Remove external information about raters and stimuli"
  )
  if (!is.null(id.info.part)) {
    dta <- dta[-id.info.part,]
    dta <- droplevels(dta)
  }
  if (!is.null(id.info.stim)) {
    dta <- dta[, -id.info.stim]
    dta <- droplevels(dta)
  }
  dta <- apply(dta, 2, as.factor)

  # Calculate the numbers of raters and stimuli
  cli::cli_bullets(
    "Calculate the numbers of raters and stimuli"
  )
  nbrater <- ncol(dta) / length(sast.parameters)
  nbstim <- nrow(dta)
  if (nbstim != sum(sast.parameters)) {
    stop(
      paste(
        "Non convenient argument",
        "-",
        "SAS paramaters does not correpond to the size of the set of stimuli"
      )
    )
  }

  # Create a res object to save the results
  res <- list()

  # Return the important arguments
  res[[1]] <- list(
    dta = dta.sauv,
    sast.parameters = sast.parameters,
    id.info.part = id.info.part,
    type.info.part = type.info.part,
    id.info.stim = id.info.stim,
    type.info.stim = type.info.stim,
    nbtimes.consens.charact = nbtimes.consens.charact,
    proba.consens.charact = proba.consens.charact,
    sep.charact = sep.charact,
    sep.part.step = sep.part.step
  )

  # Create the factorial map of the stimuli and the partition
  cli::cli_bullets(
    "Create the factorial map of the stimuli and the partition"
  )

  dta.final <- dta[, seq(3, ncol(dta), length(sast.parameters))] # only the final partitions provided by the raters
  res_stimuli_map <- get_stimuli_map(
    dta.final = dta.final,
    axis = axis
  )

  res[[2]] <- res_stimuli_map$res.mca
  coord.stim <- res_stimuli_map$coord.stim

  partition.stim <- as.integer(coord.stim$Cluster)
  names(partition.stim) <- rownames(coord.stim)
  res[[5]] <- partition.stim

  # Compute the coocurence matrix
  cli::cli_bullets(
    "Compute the coocurence matrix"
  )

  res_coocurence_matrix <- get_coocurence_matrix(
    dta.final = dta.final,
    sep.charact = sep.charact
  )

  res[[3]] <- res_coocurence_matrix$cooccur

  # Analyse the verbal characteristics
  cli::cli_bullets(
    "Analyse the verbal characteristics"
  )

  res_verbal_analysis <- get_verbal_analysis(
    res.fast = res_coocurence_matrix,
    dta.final = dta.final,
    sep.charact = sep.charact,
    id.info.stim = id.info.stim,
    dta.sauv = dta.sauv,
    dta = dta,
    type.info.stim = type.info.stim,
    nbtimes.consens.charact = nbtimes.consens.charact,
    proba.consens.charact = proba.consens.charact,
    coord.stim = coord.stim,
    axis = axis
  )

  res[[4]] <- res_verbal_analysis$res.consensualWords
  res[[6]] <- res_verbal_analysis$desc.clust.stim

  # Plot the stimuli representation
  cli::cli_bullets(
    "Plot the stimuli representation"
  )

  res_plot_stim <- plot_stim(
    res.mca = res_stimuli_map$res.mca,
    coord.stim = res_stimuli_map$coord.stim,
    coord.stim.charac = res_verbal_analysis$coord.stim.charac,
    axis = axis,
    graph = graph,
    ext.dev.Rstudio = ext.dev.Rstudio
  )

  # Define the remarkability of the stimuli
  cli::cli_bullets(
    "Define the remarkability of the stimuli"
  )

  res_stim_remarkability <- get_stim_remarkability(
    nbstim = nbstim,
    dta = dta,
    sast.parameters = sast.parameters
  )

  res[[7]] <- res_stim_remarkability

  res_plot_remark_all_stim <- plot_remark_all_stim(
    dta.remark = res_stim_remarkability,
    sast.parameters = sast.parameters,
    nbstim = nbstim,
    graph = graph,
    ext.dev.Rstudio = ext.dev.Rstudio
  )

  # Compute supplementary plots about the SAS task
  cli::cli_bullets(
    "Compute supplementary plots about the SAS task"
  )

  res_plot_nb_groups_all_steps <- plot_nb_groups_all_steps(
    dta.final = dta.final,
    dta = dta,
    sast.parameters = sast.parameters,
    graph = graph,
    ext.dev.Rstudio = ext.dev.Rstudio
  )

  res_plot_nb_stim_by_group_all_steps <- plot_nb_stim_by_group_all_steps(
    dta.final = dta.final,
    dta = dta,
    sast.parameters = sast.parameters,
    graph = graph,
    ext.dev.Rstudio = ext.dev.Rstudio
  )

  res_plot_nb_char_by_group_all_steps <- plot_nb_char_by_group_all_steps(
    dta.final = dta.final,
    dta = dta,
    sast.parameters = sast.parameters,
    sep.charact = sep.charact,
    graph = graph,
    ext.dev.Rstudio = ext.dev.Rstudio
  )

  res_plot_nb_cit_char_all_steps <- plot_nb_cit_char_all_steps(
    dta.final = dta.final,
    dta = dta,
    sast.parameters = sast.parameters,
    sep.charact = sep.charact,
    graph = graph,
    ext.dev.Rstudio = ext.dev.Rstudio
  )

  # Segment the panel of participants
  cli::cli_bullets(
    "Segment the panel of participants"
  )

  res_participants_partition <- get_participants_partition(
    dta.final = dta.final,
    sep.part.step = sep.part.step
  )

  res[[8]] <- res_participants_partition$sim.mat.part
  res[[9]] <- res_participants_partition$partition

  res_plot_participants_dendro <- plot_participants_dendro(
    mat.partition = res_participants_partition$mat.partition,
    dendrogram = res_participants_partition$dendrogram,
    graph = graph,
    ext.dev.Rstudio = ext.dev.Rstudio
  )

  res[[10]] <- list(
    data.segments = res_plot_participants_dendro$data.segments,
    data.labels = res_plot_participants_dendro$data.labels,
    dendrogram = res_plot_participants_dendro$dendrogram
  )

  # Get the consensus partition of the stimuli
  cli::cli_bullets(
    "Get the consensus partition of the stimuli"
  )

  res_consensus_partition_stim <- get_consensus_partition_stim(
    partition = res_participants_partition$partition,
    sim.mat.part = res_participants_partition$sim.mat.part,
    dta.final = dta.final,
    sep.charact = sep.charact
  )

  res[[11]] <- res_consensus_partition_stim

  # Get the description of the cluster of participants
  cli::cli_bullets(
    "Get the description of the cluster of participants"
  )

  res_desc_cluster_participants <- get_desc_cluster_participants(
    id.info.part = id.info.part,
    id.info.stim = id.info.stim,
    dta.sauv = dta.sauv,
    sast.parameters = sast.parameters,
    partition = res_participants_partition$partition,
    sep.part.step = sep.part.step,
    type.info.part = type.info.part
  )
  res[[12]] <- res_desc_cluster_participants

  # Return the results
  names(res) <- c(
    "call",
    "res.mca",
    "coocc.stim",
    "consensual.charact.stim",
    "partition.stim",
    "charact.clust.stim",
    "remark.stim",
    "ARI.part",
    "partition.part",
    "res.plot.dendro.part",
    "cons.partition.stim.clust",
    "charact.clust.part"
  )

  class(res) <- c(
    "AnalyseSAS",
    "list "
  )

  cli::cli_process_done(
    "Analysing Sequential Agglomerative Sorting task data"
  )

  return(res)

}
