plot_remark_all_stim <- function(
  dta.remark,
  sast.parameters,
  nbstim,
  graph,
  ext.dev.Rstudio
  ) {

  plots.remark <- lapply(
    1 : nbstim,
    plot_remark_one_stim,
    dta.remark = dta.remark,
    sast.parameters = sast.parameters
  )

  plot.blank <- ggplot(data.frame()) +
    geom_point() +
    theme_minimal() +
    theme(
      panel.background = element_rect(
        fill = 'transparent',
        colour = "transparent"
      )
    )
  last.plot <- length(plots.remark)
  for (i in 1:(nbstim - (floor(nbstim / 4) * 4))) {
    plots.remark[[last.plot + i]] <- plot.blank
  }

  if (graph == TRUE) {
    if ((Sys.getenv("RSTUDIO") == "1") == FALSE | ext.dev.Rstudio == TRUE) {
      dev.new(noRStudioGD = TRUE)
    }
    if (nbstim != (floor(nbstim/4) * 4)) {
      nb.wind <- floor(nbstim/4) + 1
    } else {
      nb.wind <- floor(nbstim/4)
    }
    for (i in 1 : nb.wind) {
      grid.arrange(plots.remark[[4 * i - 3]],
                   plots.remark[[4 * i - 2]],
                   plots.remark[[4 * i - 1]],
                   plots.remark[[4 * i]],
                   nrow = 2, ncol = 2)
    }
  }

  return(plots.remark)

}
