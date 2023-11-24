plot_nb_cit_char_all_steps <- function(
  dta.final,
  dta,
  sast.parameters,
  sep.charact,
  graph,
  ext.dev.Rstudio
  ) {

  plots.nb.cit.char.step <- lapply(
    1 : length(sast.parameters),
    plot_nb_cit_char_one_step,
    dta.final = dta.final,
    dta = dta,
    sast.parameters = sast.parameters,
    sep.charact = sep.charact
    )

  vec.nb.cit.char.occ.max <- vector()

  for (i in 1 : length(plots.nb.cit.char.step)) {
    vec.nb.cit.char.occ.max[i] <- plots.nb.cit.char.step[[i]][[2]]
  }
  if (graph == TRUE) {
    if ((Sys.getenv("RSTUDIO") == "1") == FALSE | ext.dev.Rstudio == TRUE) {
      dev.new(noRStudioGD = TRUE)
    }
    for (i in 1 : length(plots.nb.cit.char.step)) {
      grid.arrange(plots.nb.cit.char.step[[i]][[1]] + ylim(0, max(vec.nb.cit.char.occ.max) + 2), nrow = 1, ncol = 1)
    }
  }

  return(plots.nb.cit.char.step)

}
