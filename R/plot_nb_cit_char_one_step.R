plot_nb_cit_char_one_step <- function(
  step,
  dta.final,
  dta,
  sast.parameters,
  sep.charact
  ) {

  s <- step
  dta.step <- dta[, seq(s, ncol(dta), length(sast.parameters))]
  if (s == length(sast.parameters)) {
    dta.step <- as.vector(dta.step)
  } else {
    dta.step <- as.vector(dta.step)[-which(as.vector(dta.step) == "")]
  }
  vec.cit.char.step <- table(unlist(strsplit(dta.step, sep.charact)))
  vec.cit.char.step <- sort(vec.cit.char.step, decreasing = TRUE)
  if (length(vec.cit.char.step) > 15) {
    vec.cit.char.step <- vec.cit.char.step[1 : 15]
  }
  dta.nb.cit.char <- as.data.frame(vec.cit.char.step)
  colnames(dta.nb.cit.char) <- c("Characteristic", "Nb.cit.char")
  dta.nb.cit.char$Characteristic <- factor(dta.nb.cit.char$Characteristic, levels = levels(dta.nb.cit.char$Characteristic)[nlevels(dta.nb.cit.char$Characteristic) : 1])
  nb.cit.char.occ.max <- max(dta.nb.cit.char$Nb.cit.char)
  plot.nb.cit.char <- ggplot(NULL) +
    geom_bar(data = dta.nb.cit.char, aes(x = Characteristic, y = Nb.cit.char), stat = "identity", fill = "#E5E5E5") +
    geom_text(data = dta.nb.cit.char, aes(x = Characteristic, y = 0, label = Nb.cit.char), hjust = 1.3, color = "#444444", size = 3.5) +
    geom_text(data = dta.nb.cit.char, aes(x = Characteristic, y = 0.5, label = Characteristic), fontface = "bold", hjust = 0, color = "#444444", size = 3.5) +
    ggtitle(paste("Number of citations of the most used characteristics during the step", s)) +
    coord_flip() +
    theme(
      panel.background = element_rect(fill = 'white', colour = "#444444"),
      panel.grid.major = element_line(colour = "white"),
      panel.grid.minor = element_line(colour = "white"),
      axis.text.y = element_blank(),
      axis.text.x = element_text(colour = "#444444", size = 10),
      axis.ticks.x = element_line(colour = "#444444"),
      axis.ticks.y = element_blank(),
      axis.title = element_text(colour = "white"),
      plot.title = element_text(face = "bold", hjust = 0.5, size = 12, color = "#444444"),
      legend.position = "none")

  return(
    list(
      plot.nb.cit.char = plot.nb.cit.char,
      nb.cit.char.occ.max = nb.cit.char.occ.max
    )
  )

}
