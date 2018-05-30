plot.AnalyseSAS <- function(res, choice = "all", interact = FALSE, col.clust.stim = NULL, col.clust.part = NULL, axis = c(1, 2), ext.dev.Rstudio = FALSE, vignette = FALSE) {

  options(warn = -1)

  # load packages
  suppressPackageStartupMessages(require(grid, quietly = TRUE))
  suppressPackageStartupMessages(require(gridExtra, quietly = TRUE))
  suppressPackageStartupMessages(require(ggplot2, quietly = TRUE))
  suppressPackageStartupMessages(require(ggrepel, quietly = TRUE))
  suppressPackageStartupMessages(require(plotly, quietly = TRUE))

  # check the format of the arguments
  if (!inherits(res, "AnalyseSAS")) {
    stop("Non convenient data - res should be an AnalyseSAS object")
  }
  choice <- match.arg(choice, c("all", "stim", "part"), several.ok = TRUE)
  mat.partition.stim <- cbind.data.frame(res$partition.stim, names(res$partition.stim))
  colnames(mat.partition.stim) <- c("Cluster", "Stimulus")
  mat.partition.stim$Cluster <- as.factor(mat.partition.stim$Cluster)
  nb.clust.stim <- nlevels(mat.partition.stim$Cluster)
  if (!is.null(col.clust.stim)) {
    if (length(col.clust.stim) < nb.clust.stim) {
      stop("Non convenient specification of colors - col.clust.stim should contain at least as many elements as clusters of stimuli")
    }
  }
  mat.partition.part <- cbind.data.frame(res$partition.part, names(res$partition.part))
  colnames(mat.partition.part) <- c("Cluster", "Rater")
  mat.partition.part$Cluster <- as.factor(mat.partition.part$Cluster)
  nb.clust.part <- nlevels(mat.partition.part$Cluster)
  if (!is.null(col.clust.part)) {
    if (length(col.clust.part) < nb.clust.part) {
      stop("Non convenient specification of colors - col.clust.part should contain at least as many elements as clusters of participants")
    }
  }
  if (length(axis) != 2 | length(unique(axis)) != 2 | class(axis) != "numeric") {
    stop("Non convenient specification of axis - axis should be a numeric vector of 2 different elements")
  }

  # function to extract legend
  get.legend <- function(plot){
    tmp <- ggplot_gtable(ggplot_build(plot))
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    legend <- tmp$grobs[[leg]]
    return(legend)
  }

  # calculate the numbers of raters and stimuli
  dta.sauv <- res$call$dta
  if (!is.null(res$call$id.info.part)) {
    dta <- res$call$dta[-res$call$id.info.part,]
    dta <- droplevels(dta)
  }
  if (!is.null(res$call$id.info.stim)) {
    dta <- dta[, -res$call$id.info.stim]
    dta <- droplevels(dta)
  }
  if(is.null(res$call$id.info.part) & is.null(res$call$id.info.stim)) {
    dta <- res$call$dta
    dta <- droplevels(dta)
  }
  nbrater <- ncol(dta) / length(res$call$sast.parameters)
  nbstim <- nrow(dta)
  dta.final <- dta[, seq(3, ncol(dta), by = length(res$call$sast.parameters))]

  # stimulus-oriented analysis
  if (!is.na(match("all", choice)) | !is.na(match("stim", choice))) {

    if (is.null(col.clust.stim)) {
      col.clust.stim <- c("#90B08F", "#EA485C", "#FF8379", "#009193", "#FFCEA5", "#A9A9A9", "#B0983D", "#941751", "#333333", "#A8D9FF")
    }
    palette.col <- col.clust.stim

    res.mca <- res$res.mca
    coord.stim <- as.data.frame(res.mca$ind$coord[, axis])
    coord.stim <- as.data.frame(res.mca$ind$coord[, axis])
    res.hcpc <- HCPC(res.mca, nb.clust = -1, graph = FALSE)
    vec.clust <- as.data.frame(res.hcpc$data.clust$clust)
    rownames(vec.clust) <- rownames(res.hcpc$data.clust)
    colnames(vec.clust) <- "Cluster"
    coord.stim <- merge(coord.stim, vec.clust, by = "row.names")
    rownames(coord.stim) <- coord.stim[, 1]
    coord.stim <- coord.stim[, -1]
    colnames(coord.stim)[1 : 2] <- c("AxeA", "AxeB")

    wordsCons <- res$consensual.charact.stim$Consensual.words
    coord.charac <- cbind.data.frame(res$consensual.charact.stim$Centroids[which(rownames(res$consensual.charact.stim$Centroids)%in%wordsCons), axis],
                                     Cluster = rep(as.character(nlevels(coord.stim$Cluster)+1), length(wordsCons)))
    colnames(coord.charac)[1:2] <- c("AxeA", "AxeB")
    coord.stim.charac <- rbind.data.frame(coord.stim, coord.charac)
    coord.stim.charac <- cbind.data.frame(coord.stim.charac, c(rep("stimulus", nrow(coord.stim)), rep("characteristic", nrow(coord.charac))))
    colnames(coord.stim.charac)[4] <- "Type"
    coord.stim.charac$Type <- relevel(coord.stim.charac$Type, ref = "stimulus")

    if (interact == FALSE) {

      plot.stim <- ggplot(NULL) +
        labs(x = paste("Dim ", 1," - ", round(res.mca$eig[axis[1], 2], 2), " %", sep = ""), y = paste("Dim ", 2, " - ", round(res.mca$eig[axis[2], 2], 2), " %", sep = "")) +
        coord_fixed()+
        geom_hline(yintercept = 0, linetype = 2, color = "#444444", size = 0.2) +
        geom_vline(xintercept = 0,  linetype = 2, color = "#444444", size = 0.2) +
        geom_point(data = coord.stim.charac, aes(AxeA, AxeB, color = Cluster, shape = Type)) +
        geom_text_repel(data = coord.stim.charac, aes(x = AxeA, y = AxeB, label = rownames(coord.stim.charac), color = Cluster), segment.color = "#444444", segment.size = 0.3, size = 2.3) +
        scale_color_manual(values = c(palette.col[1 : nlevels(coord.stim$Cluster)], "#444444")) +
        ggtitle("Stimuli representation") +
        theme(
          panel.background = element_rect(fill = 'white', colour = "#444444"),
          panel.grid.major = element_line(colour = "white"),
          panel.grid.minor = element_line(colour = "white"),
          axis.text = element_text(colour = "#444444"),
          axis.ticks = element_line(colour = "#444444"),
          axis.title = element_text(colour = "#444444"),
          plot.title = element_text(face = "bold", hjust = 0.5, size = 12, color = "#444444"),
          legend.position = "none"
        )
      data.legend.clust <- cbind.data.frame(rep(1, length(coord.stim$Cluster)), coord.stim$Cluster)
      colnames(data.legend.clust) <- c("x", "Cluster")
      plot.legend.clust.stim <- ggplot(NULL) +
        geom_label(data = data.legend.clust, aes(label = Cluster, x = x, y = -0.1, angle = 90, hjust = 1, fill = Cluster), colour = "transparent") +
        scale_fill_manual(values = palette.col[1 : nlevels(data.legend.clust$Cluster)]) +
        theme(
          plot.margin = unit(c(0,0,0,0), "cm"),
          legend.position = "bottom",
          legend.title = element_text(size=8, colour = "#444444"),
          legend.text = element_text(size=8, colour = "#444444"),
          legend.margin = margin(t=0, unit='cm'),
          legend.key = element_rect(size=4),
          legend.key.size = unit(0.4, "cm"))
      legend.plot.stim <- get.legend(plot.legend.clust.stim)
      if ((Sys.getenv("RSTUDIO") == "1") == FALSE | ext.dev.Rstudio == TRUE) {
        dev.new(noRStudioGD = TRUE)
      }
      grid.arrange(arrangeGrob(plot.stim + theme(legend.position="none"),
                               ncol = 1, nrow = 1),
                   legend.plot.stim, nrow = 2, heights = c(8, 1))

    } else if (interact == TRUE) {

      text.tooltip.stim <- paste("Stimulus:", rownames(coord.stim), '<br>Cluster:', gsub("Cluster ", "", coord.stim$Cluster))

      dta.info.stim.remark <- as.data.frame(res$remark.stim[, 1])
      rownames(dta.info.stim.remark) <- rownames(res$remark.stim)
      dta.info.stim.remark <- merge(coord.stim, dta.info.stim.remark, by = "row.names")
      text.tooltip.stim <- paste(text.tooltip.stim, paste("Remarkability during the SAS task:", dta.info.stim.remark[, ncol(dta.info.stim.remark)]), sep = " <br>")

      if (!is.null(res$call$id.info.stim)) {
        dta.info.stim <- as.data.frame(dta.sauv[1 : nbstim, res$call$id.info.stim])
        colnames(dta.info.stim) <- colnames(dta.sauv)[id.info.stim]
        rownames(dta.info.stim) <- rownames(dta)
        dta.info.stim <- merge(coord.stim, dta.info.stim, by = "row.names")
        rownames(dta.info.stim) <- dta.info.stim[, 1]
        dta.info.stim <- dta.info.stim[, -(2 : 4)]
        colnames(dta.info.stim)[1] <- "Stimulus"
        for (i in 2 : ncol(dta.info.stim)) {
          text.tooltip.stim <- paste(text.tooltip.stim, paste0("<br>", colnames(dta.info.stim)[i], ": ", as.character(dta.info.stim[,i])))
        }
      }

      dta.charact.by.stim <- as.data.frame(matrix(NA, nbstim, 1))
      rownames(dta.charact.by.stim) <- rownames(dta.final)
      colnames(dta.charact.by.stim) <- "Description"
      for (i in 1 : nrow(dta.charact.by.stim)) {
        dta.charact.by.stim[i, 1] <- paste(unlist(strsplit(as.character(unlist(as.vector(dta.final[i,]))), res$call$sep.charact)), collapse = " ; ")
      }
      dta.charact.by.stim <- cbind.data.frame(rownames(dta.charact.by.stim), dta.charact.by.stim)
      colnames(dta.charact.by.stim)[1] <- c("Stimulus")
      res.textual <- textual(dta.charact.by.stim, num.text = 2, contingence.by = 1, sep.word = " ; ")
      res.textual$cont.table <- res.textual$cont.table[, -which(apply(res.textual$cont.table, 2, "sum") < res$call$nbtimes.consens.charact)]
      res.desc.clust.stim.text <- descfreq(res.textual$cont.table)
      dta.info.stim.text <- as.data.frame(matrix(NA, nbstim, 1))
      colnames(dta.info.stim.text) <- "Description"
      rownames(dta.info.stim.text) <- names(res.desc.clust.stim.text)
      for (i in 1 : nrow(dta.info.stim.text)) {
        if (!is.null(res.desc.clust.stim.text[[i]])) {
          sign.pval <- rep(NA, nrow(res.desc.clust.stim.text[[i]]))
          sign.pval[which(res.desc.clust.stim.text[[i]][, "v.test"] > 0)] <- "+"
          sign.pval[which(res.desc.clust.stim.text[[i]][, "v.test"] < 0)] <- "-"
          dta.info.stim.text[i, 1] <- paste(paste(sign.pval, rownames(res.desc.clust.stim.text[[i]])), collapse = " <br>")
        }
      }
      dta.info.stim.text[which(is.na(dta.info.stim.text[, "Description"])), "Description"] <- ""
      text.tooltip.stim <- paste(text.tooltip.stim, dta.info.stim.text[, "Description"], sep = " <br>")

      coord.stim$Cluster <- paste("Cluster", coord.stim$Cluster)
      col.interact.stim <- palette.col[1 : nlevels(as.factor(coord.stim$Cluster))]

      text.tooltip.char <- rownames(coord.charac)

      plot.stim.interact <- plot_ly() %>%
        add_trace(coord.stim,
                  x = ~coord.stim$AxeA ,
                  y = ~coord.stim$AxeB,
                  color = ~coord.stim$Cluster,
                  hoverinfo = 'text',
                  text = text.tooltip.stim,
                  type = "scatter", mode = "markers",
                  marker = list(size = 6, symbol = "circle"),
                  showlegend = TRUE,
                  colors = col.interact.stim,
                  legendgroup = 'group1') %>%
        add_trace(coord.charac,
                  x = ~coord.charac$AxeA ,
                  y = ~coord.charac$AxeB,
                  hoverinfo = 'text',
                  text = text.tooltip.char,
                  type = "scatter", mode = "markers",
                  marker = list(size = 6, symbol = "triangle-up", color = "#444444"),
                  name = "characteristic",
                  showlegend = TRUE,
                  legendgroup = 'group2') %>%
        layout(
          legend = list(orientation = "h", xanchor = "center", x = 0.5, y = -0.25),
          margin = list(l = 50, r = 50, t = 50, b = 50),
          titlefont = list(size = 16, color = "#444444"),
          title = "Stimuli representation",
          xaxis = list(zerolinecolor = "#D6D5D5", scaleanchor = "y", showgrid = FALSE, title = paste("Dim ", axis[1], " - ", round(res.mca$eig[axis[1],2],2), "%", sep=""), titlefont = list(color = "#444444", size = 13), tickfont = list(size = 10, color = "#444444"), showline = TRUE, mirror = "ticks", linecolor = "#444444", linewidth = 1),
          yaxis = list(zerolinecolor = "#D6D5D5", scaleanchor = "x", showgrid = FALSE, title = paste("Dim ", axis[2], " - ", round(res.mca$eig[axis[2],2],2), "%", sep=""), titlefont = list(color = "#444444", size = 13), tickfont = list(size = 10, color = "#444444"), showline = TRUE, mirror = "ticks", linecolor = "#444444", linewidth = 1)
        )
      if (vignette == FALSE) {
        if (ext.dev.Rstudio == TRUE) {
          old.viewer <- options()$viewer
          options(viewer = NULL)
          print(plot.stim.interact)
          options(viewer = old.viewer)
        } else {
          print(plot.stim.interact)
        }
      }
    }
  }

  # participant-oriented analysis
  if (!is.na(match("all", choice)) | !is.na(match("part", choice))) {

    if (is.null(col.clust.part)) {
      col.clust.part <- c("#90B08F", "#EA485C", "#FF8379", "#009193", "#FFCEA5", "#A9A9A9", "#B0983D", "#941751", "#333333", "#A8D9FF")
    }
    palette.col <- col.clust.part

    if (interact == FALSE) {

      plot.dendro <- ggplot(NULL) +
        geom_segment(data = res$res.plot.dendro.part$data.segments, aes(x = x, y = y, xend = xend, yend = yend), colour = "#444444") +
        geom_text(data = res$res.plot.dendro.part$data.labels, aes(label = Rater, x = x, y = -0.1, angle = 90, hjust = 1, colour = Cluster), size = 2.1) +
        scale_colour_manual(values = palette.col[1 : nlevels(res$res.plot.dendro.part$data.labels$Cluster)]) +
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
        geom_label(data = res$res.plot.dendro.part$data.labels, aes(label = Rater, x = x, y = -0.1, angle = 90, hjust = 1, fill = Cluster), colour = "transparent") +
        scale_fill_manual(values = palette.col[1 : nlevels(res$res.plot.dendro.part$data.labels$Cluster)]) +
        theme(
          plot.margin = unit(c(0,0,0,0), "cm"),
          legend.position = "bottom",
          legend.title = element_text(size=8, colour = "#444444"),
          legend.text = element_text(size=8, colour = "#444444"),
          legend.margin = margin(t=0, unit='cm'),
          legend.key = element_rect(size=4),
          legend.key.size = unit(0.4, "cm"))
      legend.plot.part <- get.legend(plot.legend.clust.part)
      if ((Sys.getenv("RSTUDIO") == "1") == FALSE | ext.dev.Rstudio == TRUE) {
        dev.new(noRStudioGD = TRUE)
      }
      grid.arrange(arrangeGrob(plot.dendro + theme(legend.position="none"),
                               ncol = 1, nrow = 1),
                   legend.plot.part, nrow = 2, heights = c(8, 1))
    } else if (interact == TRUE) {

      res$res.plot.dendro.part$data.labels$Cluster <- as.factor(paste("Cluster", res$res.plot.dendro.part$data.labels$Cluster))

      plot.dendro <- ggplot(NULL) +
        geom_segment(data = res$res.plot.dendro.part$data.segments, aes(x = x, y = y, xend = xend, yend = yend), colour = "#444444") +
        geom_text(data = res$res.plot.dendro.part$data.labels, aes(label = Rater, x = x, y = -0.15, angle = 90, hjust = 1), size = 2.1) +
        geom_point(data = res$res.plot.dendro.part$data.labels, aes(x = x, y = -0.1, colour = Cluster), size = 2) +
        scale_colour_manual(values = palette.col[1 : nlevels(res$res.plot.dendro.part$data.labels$Cluster)]) +
        ggtitle("Participants clustering") +
        theme(
          legend.key = element_rect(colour = "white", fill = "white"),
          legend.title = element_blank(),
          legend.text = element_text(colour = "#444444"),
          panel.background = element_rect(fill = 'white', colour = "white"),
          panel.grid.major = element_line(colour = "white"),
          panel.grid.minor = element_line(colour = "white"),
          plot.margin = unit(c(0.5,0,0,0), "cm"),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.x = element_blank())

      plot.part.interact <- ggplotly(plot.dendro)  %>%
        layout(
          legend = list(orientation = "h", xanchor = "center", x = 0.5, y = -3),
          margin = list(l = 50, r = 50, t = 50, b = 50),
          titlefont = list(size = 16, color = "#444444", facefont = "bold"),
          title = "Participants clustering"
        )

      plot.part.interact$x$data[[1]]$text <- rep(NA, length(plot.part.interact$x$data[[1]]$text))
      plot.part.interact$x$data[[2]]$hovertext <- rep(NA, length(plot.part.interact$x$data[[2]]$hovertext))
      for (i in 1 : nlevels(res$res.plot.dendro.part$data.labels$Cluster)) {
        part.clust <- res$res.plot.dendro.part$data.labels[which(res$res.plot.dendro.part$data.labels$Cluster == paste("Cluster", i)), "Rater"]
        text.tooltip.part.clust <- rep(paste("Cluster:", i), length(part.clust))
        if (!is.null(res$call$id.info.part)) {
          dta.info.sup.part <- dta.sauv[res$call$id.info.part, seq(1, ncol(dta.sauv), length(res$call$sast.parameters))]
          dta.info.sup.part <- dta.info.sup.part[, 1 : nbrater]
          colnames(dta.info.sup.part) <- gsub(paste0(res$call$sep.part.step, ".*$"), "", colnames(dta.info.sup.part))
          dta.info.sup.part <- t(dta.info.sup.part[, which(colnames(dta.info.sup.part) %in% part.clust)])
          dta.plotly <- res$res.plot.dendro.part$data.labels
          rownames(dta.plotly) <- dta.plotly[, 1]
          dta.info.sup.part <- merge(dta.plotly[which(rownames(dta.plotly) %in% part.clust),], dta.info.sup.part, by = "row.names")
          dta.info.sup.part <- dta.info.sup.part[, -(1 : 5)]
          for (j in 1 : ncol(dta.info.sup.part)) {
            text.tooltip.part.clust <- paste(text.tooltip.part.clust,
                                             paste(colnames(dta.info.sup.part)[j], dta.info.sup.part[, j], sep = ": "),
                                             sep = " <br>")
          }
        }
        plot.part.interact$x$data[[2 + i]]$text <- text.tooltip.part.clust
      }
      if (vignette == FALSE) {
        if (ext.dev.Rstudio == TRUE) {
          old.viewer <- options()$viewer
          options(viewer = NULL)
          print(plot.part.interact)
          options(viewer = old.viewer)
        } else {
          print(plot.part.interact)
        }
      }

    }

  }

  # end the function
  options(warn = 0)
  return(message("Representations plotted"))

}
