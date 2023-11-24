plot_nb_stim_by_group_all_steps <- function(
  dta.final,
  dta,
  sast.parameters,
  graph,
  ext.dev.Rstudio
  ) {

  plots.nb.stim.step <- lapply(
    1 : length(sast.parameters),
    plot_nb_stim_by_group_one_step,
    dta.final = dta.final,
    dta = dta,
    sast.parameters = sast.parameters
    )

  vec.nb.stim.occ.max <- vector()
  for (i in 1 : length(plots.nb.stim.step)) {
    vec.nb.stim.occ.max[i] <- plots.nb.stim.step[[i]][[2]]
  }
  last.plot <- length(plots.nb.stim.step)
  if ((length(sast.parameters) - (floor(length(sast.parameters)/3) * 3)) != 0) {
    for (i in 1 : (length(sast.parameters) - (floor(length(sast.parameters)/3) * 3))) {
      plots.nb.stim.step[[last.plot+i]] <- plot.blank
    }
  }
  if (graph == TRUE) {
    if ((Sys.getenv("RSTUDIO") == "1") == FALSE | ext.dev.Rstudio == TRUE) {
      dev.new(noRStudioGD = TRUE)
    }
    if (length(sast.parameters) != (floor(length(sast.parameters)/3) * 3)) {
      nb.wind <- floor(length(sast.parameters)/3) + 1
    } else {
      nb.wind <- floor(length(sast.parameters)/3)
    }
    for (i in 1 : nb.wind) {
      grid.arrange(plots.nb.stim.step[[3 * i - 2]][[1]] + ylim(0, max(vec.nb.stim.occ.max) + 2),
                   plots.nb.stim.step[[3 * i - 1]][[1]] + ylim(0, max(vec.nb.stim.occ.max) + 2),
                   plots.nb.stim.step[[3 * i]][[1]] + ylim(0, max(vec.nb.stim.occ.max) + 2),
                   nrow = 3, ncol = 1)
    }
  }

  return(
    plots.nb.stim.step
  )

}

