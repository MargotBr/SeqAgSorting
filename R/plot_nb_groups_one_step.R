plot_nb_groups_one_step <- function(
  step,
  dta.final,
  dta,
  sast.parameters
  ) {

  vec.group.final <- vector()

  nbrater <- ncol(dta.final)

  for (i in 1 : nbrater) {
    vec.group.final[i] <- nlevels(as.factor(dta.final[which(dta.final[, i] != ""), i]))
  }

  nb.group.max <- max(vec.group.final)

  s <- step

  dta.step <- dta[, seq(s, ncol(dta), length(sast.parameters))]

  vec.group.step <- vector()

  for (i in 1 : nbrater) {
    vec.group.step[i] <- nlevels(as.factor(dta.step[which(dta.step[, i] != ""), i]))
  }

  dta.nb.groups <- cbind.data.frame(Nb.group = 1 : nb.group.max,
                                    Effectif = rep(0, nb.group.max))

  for (i in 1 : nrow(dta.nb.groups)) {
    dta.nb.groups[i, 2] <- length(which(vec.group.step == i))
  }

  nb.group.occ.max <- max(dta.nb.groups$Effectif)

  plot.nb.groups <- ggplot(NULL) +
    geom_bar(data = dta.nb.groups, aes(x = Nb.group, y = Effectif), stat = "identity", fill = "#444444") +
    geom_text(data = dta.nb.groups, aes(x = Nb.group, y = Effectif, label = Effectif), vjust = -0.5, color = "#444444", size = 3.5) +
    ggtitle(paste("Number of groups formed during the step", s)) +
    scale_x_discrete(limits = factor(1 : nb.group.max)) +
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
      plot.nb.groups = plot.nb.groups,
      nb.group.occ.max = nb.group.occ.max
    )
  )

}
