plot_stim <- function(
  res.mca,
  coord.stim,
  coord.stim.charac,
  axis,
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

  plot.stim <- ggplot(NULL) +
    labs(
      x = paste(
        "Dim ", 1," - ", round(res.mca$eig[axis[1], 2], 2), " %", sep = ""
      ),
      y = paste(
        "Dim ", 2, " - ", round(res.mca$eig[axis[2], 2], 2), " %", sep = ""
      )
    ) +
    coord_fixed() +
    geom_hline(yintercept = 0, linetype = 2, color = "#444444", size = 0.2) +
    geom_vline(xintercept = 0,  linetype = 2, color = "#444444", size = 0.2) +
    geom_point(
      data = coord.stim.charac,
      aes(AxeA, AxeB, color = Cluster, shape = Type)
    ) +
    geom_text_repel(
      data = coord.stim.charac,
      aes(
        x = AxeA,
        y = AxeB,
        label = rownames(coord.stim.charac),
        color = Cluster
      ),
      segment.color = "#444444",
      segment.size = 0.3,
      size = 2.3
    ) +
    scale_color_manual(
      values = c(palette.col[1 : nlevels(coord.stim$Cluster)], "#444444")
    ) +
    ggtitle("Stimuli representation") +
    theme(
      panel.background = element_rect(fill = 'white', colour = "#444444"),
      panel.grid.major = element_line(colour = "white"),
      panel.grid.minor = element_line(colour = "white"),
      axis.text = element_text(colour = "#444444"),
      axis.ticks = element_line(colour = "#444444"),
      axis.title = element_text(colour = "#444444"),
      plot.title = element_text(
        face = "bold", hjust = 0.5, size = 12, color = "#444444"
      ),
      legend.position = "none"
    )

  data.legend.clust <- cbind.data.frame(
    rep(1, length(coord.stim$Cluster)),
    coord.stim$Cluster
  )
  colnames(data.legend.clust) <- c("x", "Cluster")

  plot.legend.clust.stim <- ggplot(NULL) +
    geom_label(
      data = data.legend.clust,
      aes(
        label = Cluster,
        x = x,
        y = -0.1,
        angle = 90,
        hjust = 1,
        fill = Cluster
      ),
      colour = "transparent"
    ) +
    scale_fill_manual(
      values = palette.col[1 : nlevels(data.legend.clust$Cluster)]
    ) +
    theme(
      plot.margin = unit(c(0,0,0,0), "cm"),
      legend.position = "bottom",
      legend.title = element_text(size=8, colour = "#444444"),
      legend.text = element_text(size=8, colour = "#444444"),
      legend.margin = margin(t=0, unit='cm'),
      legend.key = element_rect(size=4),
      legend.key.size = unit(0.4, "cm"))

  if ((Sys.getenv("RSTUDIO") == "1") == FALSE) {
    empty.dev <- (dev.cur() == 1)
  }

  legend.plot.stim <- get_legend(plot.legend.clust.stim)

  if ((Sys.getenv("RSTUDIO") == "1") == FALSE) {
    if (empty.dev == TRUE) {
      dev.off()
    }
  }

  if (graph == TRUE) {
    if ((Sys.getenv("RSTUDIO") == "1") == FALSE | ext.dev.Rstudio == TRUE) {
      dev.new(noRStudioGD = TRUE)
    }
    grid.arrange(arrangeGrob(plot.stim + theme(legend.position="none"),
                             ncol = 1, nrow = 1),
                 legend.plot.stim, nrow = 2, heights = c(8, 1))
  }

  return(plot.stim)

}
