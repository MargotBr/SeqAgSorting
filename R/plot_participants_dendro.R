plot_participants_dendro <- function(
  mat.partition,
  dendrogram,
  graph,
  ext.dev.Rstudio
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

  dendrogram.info <- as.dendrogram(dendrogram)

  dendrogram.data <- dendro_data(dendrogram.info)
  data.labels <- label(dendrogram.data)

  colnames(data.labels)[3] <- "Rater"

  data.labels <- merge(data.labels, mat.partition, by = "Rater")

  data.labels$Cluster <- as.factor(data.labels$Cluster)
  data.segments <- segment(dendrogram.data)
  plot.dendro <- ggplot(NULL) +
    geom_segment(data = data.segments, aes(x = x, y = y, xend = xend, yend = yend), colour = "#444444") +
    geom_text(data = data.labels, aes(label = Rater, x = x, y = -0.1, angle = 90, hjust = 1, colour = Cluster), size = 2.1) +
    scale_colour_manual(values = palette.col[1 : nlevels(data.labels$Cluster)]) +
    ggtitle("Participants clustering") +
    theme(
      legend.key = element_rect(colour = "white", fill = "white"),
      legend.title = element_text(colour = "#444444"),
      legend.text = element_text(colour = "#444444"),
      panel.background = element_rect(fill = 'white', colour = "white"),
      panel.grid.major = element_line(colour = "white"),
      panel.grid.minor = element_line(colour = "white"),
      plot.title = element_text(face = "bold", hjust = 0.5, vjust = -1, size = 12, colour = "#444444"),
      plot.margin = unit(c(0.5,0,0,0), "cm"),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.ticks.x = element_blank(),
      axis.text.x = element_blank(),
      legend.position = "none")

  plot.legend.clust.part <- ggplot(NULL) +
    geom_label(data = data.labels, aes(label = Rater, x = x, y = -0.1, angle = 90, hjust = 1, fill = Cluster), colour = "transparent") +
    scale_fill_manual(values = palette.col[1 : nlevels(data.labels$Cluster)]) +
    theme(
      plot.margin = unit(c(0,0,0,0), "cm"),
      legend.position = "bottom",
      legend.title = element_text(size=8, colour = "#444444"),
      legend.text = element_text(size=8, colour = "#444444"),
      legend.margin = margin(t=0, unit='cm'),
      legend.key = element_rect(size=4),
      legend.key.size = unit(0.4, "cm"))
  legend.plot.part <- get_legend(plot.legend.clust.part)
  if (graph == TRUE) {
    if ((Sys.getenv("RSTUDIO") == "1") == FALSE | ext.dev.Rstudio == TRUE) {
      dev.new(noRStudioGD = TRUE)
    }
    grid.arrange(arrangeGrob(plot.dendro + theme(legend.position="none"),
                             ncol = 1, nrow = 1),
                 legend.plot.part, nrow = 2, heights = c(8, 1))
  }

  return(
    list(
      plot.dendro = plot.dendro,
      data.segments = data.segments,
      data.labels = data.labels,
      dendrogram
    )
  )

}
