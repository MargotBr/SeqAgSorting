plot_nb_char_by_group_one_step <- function(
  step,
  dta.final,
  dta,
  sast.parameters,
  sep.charact
  ) {

  nbrater <- ncol(dta.final)

  vec.char.final <- vector()
  for (i in 1 : nbrater) {
    list.char.final <- strsplit(levels(as.factor(dta.final[,i])), sep.charact)
    vec.char.final.rat <- vector()
    for (j in 1 : length(list.char.final)) {
      vec.char.final.rat[j] <- length(list.char.final[[j]])
    }
    vec.char.final[(length(vec.char.final) + 1) : (length(vec.char.final) + length(vec.char.final.rat))] <- vec.char.final.rat
  }
  nb.char.max <- max(vec.char.final)
  s <- step
  dta.step <- dta[, seq(s, ncol(dta), length(sast.parameters))]
  vec.char.step <- vector()
  for (i in 1 : nbrater) {
    list.char.step <- strsplit(levels(as.factor(dta.step[which(dta.step[, i] != ""), i])), sep.charact)
    vec.char.step.rat <- vector()
    for (j in 1 : length(list.char.step)) {
      vec.char.step.rat[j] <- length(list.char.step[[j]])
    }
    vec.char.step[(length(vec.char.step) + 1) : (length(vec.char.step) + length(vec.char.step.rat))] <- vec.char.step.rat
  }
  dta.nb.char <- cbind.data.frame(Nb.char = 1 : nb.char.max,
                                  Effectif = rep(0, nb.char.max))
  for (i in 1 : nrow(dta.nb.char)) {
    dta.nb.char[i, 2] <- length(which(vec.char.step == i))
  }
  nb.char.occ.max <- max(dta.nb.char$Effectif)
  plot.nb.char <- ggplot(NULL) +
    geom_bar(data = dta.nb.char, aes(x = Nb.char, y = Effectif), stat = "identity", fill = "#444444") +
    geom_text(data = dta.nb.char, aes(x = Nb.char, y = Effectif, label = Effectif), vjust = -0.5, color = "#444444", size = 3.5) +
    ggtitle(paste("Number of characteristics cited per group during the step", s)) +
    scale_x_discrete(limits = factor(1 : nb.char.max)) +
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
      plot.nb.char = plot.nb.char,
      nb.char.occ.max = nb.char.occ.max
    )
  )

}
