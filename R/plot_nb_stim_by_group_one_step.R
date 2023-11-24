plot_nb_stim_by_group_one_step <- function(
    step,
    dta.final,
    dta,
    sast.parameters
  ) {

  vec.stim.final <- vector()

  nbrater <- ncol(dta.final)

  for (i in 1 : nbrater) {

    vec.stim.final[(length(vec.stim.final) + 1) : (length(vec.stim.final) + length(table(dta.final[,i])))] <- table(dta.final[,i])
  }

  nb.stim.max <- max(vec.stim.final)
  s <- step

  dta.step <- dta[, seq(s, ncol(dta), length(sast.parameters))]
  vec.stim.step <- vector()
  for (i in 1 : nbrater) {
    vec.stim.step[(length(vec.stim.step) + 1) : (length(vec.stim.step) + (length(table(dta.step[,i])[which(names(table(dta.step[,i])) != "")])))] <- table(dta.step[,i])[which(names(table(dta.step[,i])) != "")]
  }
  dta.nb.stim <- cbind.data.frame(Nb.stim = 1 : nb.stim.max,
                                  Effectif = rep(0, nb.stim.max))
  for (i in 1 : nrow(dta.nb.stim)) {
    dta.nb.stim[i, 2] <- length(which(vec.stim.step == i))
  }
  nb.stim.occ.max <- max(dta.nb.stim$Effectif)
  plot.nb.stim <- ggplot(NULL) +
    geom_bar(data = dta.nb.stim, aes(x = Nb.stim, y = Effectif), stat = "identity", fill = "#444444") +
    geom_text(data = dta.nb.stim, aes(x = Nb.stim, y = Effectif, label = Effectif), vjust = -0.5, color = "#444444", size = 3.5) +
    ggtitle(paste("Number of stimuli per group during the step", s)) +
    scale_x_discrete(limits = factor(1 : nb.stim.max)) +
    theme(
      panel.background = element_rect(fill = 'white', colour = "#444444"),
      panel.grid.major = element_line(colour = "white"),
      panel.grid.minor = element_line(colour = "white"),
      axis.text = element_text(colour = "#444444", size = 10),
      axis.ticks = element_line(colour = "#444444"),
      axis.title = element_text(colour = "white"),
      plot.title = element_text(face = "bold", hjust = 0.5, size = 12, color = "#444444"),
      legend.position = "none")

  return(
    list(
      plot.nb.stim = plot.nb.stim,
      nb.stim.occ.max = nb.stim.occ.max
    )
  )

}
