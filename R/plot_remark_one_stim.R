plot_remark_one_stim <- function(
  dta.remark,
  stim,
  sast.parameters
  ){

  palette.col <- c(
    "#90B08F",
    "#EA485C",
    "#FF8379",
    "#009193",
    "#FFCEA5",
    "#A9A9A9",
    "#B0983D",
    "#941751",
    "#333333",
    "#A8D9FF"
  )

  dta.remark.step <- cbind.data.frame(
    1 : length(sast.parameters),
    t(dta.remark[stim, seq(2, ncol(dta.remark), by = 2)]),
    t(dta.remark[stim, seq(3, ncol(dta.remark), by = 2)])
  )
  colnames(dta.remark.step) <- c("Step", "Contribution", "Type")

  dta.remark.step$Type <- as.factor(dta.remark.step$Type)

  if (all(levels(dta.remark.step$Type) == "R")) {
    palette.col.remark <- palette.col[2]
  } else {
    palette.col.remark <- palette.col[1 : 2]
  }
  if (
    (dta.remark[stim, 1] == "Insignificant -> Remarkable") | (dta.remark[stim, 1] == "Remarkable -> Insignificant")
    ) {
    col.background = "#E5E5E5"
  } else {
    col.background = "white"
  }

  max.contrib <- max(
    apply(dta.remark[, which(colnames(dta.remark) %in% grep('contrib', colnames(dta.remark), value=TRUE))], 2, max)
  )

  plot.remark <- ggplot(NULL) +
    ylim(0, max.contrib + 5) +
    geom_line(data = dta.remark.step, aes(Step, Contribution), color = "#444444") +
    geom_point(data = dta.remark.step, aes(Step, Contribution, color = Type), size = 4) +
    geom_text(data = dta.remark.step, aes(x = Step, y = (Contribution + 3), label = Type, color = Type), size = 4) +
    scale_x_discrete(
      labels = paste("Step", 1 : length(sast.parameters)),
      limits = factor(1 : 3)
    ) +
    scale_color_manual(values = palette.col.remark) +
    ggtitle(paste("Evolution of", rownames(dta.remark)[stim], ":", dta.remark[stim, 1])) +
    theme(
      panel.background = element_rect(fill = col.background, colour = "#444444"),
      panel.grid.major = element_line(colour = col.background),
      panel.grid.minor = element_line(colour = col.background),
      axis.text.y = element_text(colour = "#444444"),
      axis.text.x = element_text(colour = "#444444", size = 10),
      axis.ticks = element_line(colour = "#444444"),
      axis.title.y = element_text(colour = "#444444"),
      axis.title.x = element_blank(),
      plot.title = element_text(face = "bold", hjust = 0.5, size = 11, color = "#444444"),
      legend.position = "none"
    )

  return(plot.remark)

}
