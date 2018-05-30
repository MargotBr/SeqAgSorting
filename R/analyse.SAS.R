analyse.SAS <- function(dta, sast.parameters, sep.part.step = "_", sep.charact = " ; ", nbtimes.consens.charact = 5, proba.consens.charact = 0.05, id.info.stim = NULL, type.info.stim = NULL, id.info.part = NULL, type.info.part = NULL, axis = c(1, 2), graph = TRUE, ext.dev.Rstudio = FALSE,...) {
  
  options(warn = -1)
  
  # load packages
  suppressPackageStartupMessages(require(FactoMineR, quietly = TRUE))
  suppressPackageStartupMessages(require(SensoMineR, quietly = TRUE))
  suppressPackageStartupMessages(require(ggplot2, quietly = TRUE))
  suppressPackageStartupMessages(require(gridExtra, quietly = TRUE))
  suppressPackageStartupMessages(require(ggrepel, quietly = TRUE))
  suppressPackageStartupMessages(require(FreeSortR, quietly = TRUE))
  suppressPackageStartupMessages(require(dendextend, quietly = TRUE))
  suppressPackageStartupMessages(require(ggdendro, quietly = TRUE))
  suppressPackageStartupMessages(require(NbClust, quietly = TRUE))
  
  # save the data set
  dta <- as.data.frame(dta)
  dta.sauv <- dta
  
  # function to extract legend
  get.legend <- function(plot){
    tmp <- ggplot_gtable(ggplot_build(plot))
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    legend <- tmp$grobs[[leg]]
    return(legend)
  }
  
  # remove external information about raters and stimuli
  if (!is.null(id.info.part)) {
    dta <- dta[-id.info.part,]
    dta <- droplevels(dta)
  }
  if (!is.null(id.info.stim)) {
    dta <- dta[, -id.info.stim]
    dta <- droplevels(dta)
  }
  dta <- apply(dta, 2, as.factor)
  
  # calculate the numbers of raters and stimuli
  nbrater <- ncol(dta) / length(sast.parameters)
  nbstim <- nrow(dta)
  
  # create a res object to save the results
  res <- list()
  
  # return the important arguments
  res[[1]] <- list(dta.sauv, sast.parameters, id.info.part, type.info.part, id.info.stim, type.info.stim, nbtimes.consens.charact, proba.consens.charact, sep.charact, sep.part.step)
  names(res[[1]]) <- c("dta", "sast.parameters", "id.info.part", "type.info.part", "id.info.stim", "type.info.stim", "nbtimes.consens.charact", "proba.consens.charact", "sep.charact", "sep.part.step")
  
  # create the factorial map of the stimuli
  dta.final <- dta[, seq(3, ncol(dta), length(sast.parameters))] # only the final partitions provided by the raters
  res.mca <- MCA(dta.final, graph = FALSE)
  res[[2]] <- res.mca
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
  
  # compute the coocurence matrix
  res.fast <- fast(dta.final, sep.words = sep.charact, graph = FALSE)
  res[[3]] <- res.fast$cooccur
  
  # analyse the verbal characteristics
  res.consensualWords <- ConsensualWords(res.fast, nbtimes = nbtimes.consens.charact, proba = proba.consens.charact, graph = FALSE)
  res[[4]] <- res.consensualWords
  wordsCons <- res.consensualWords$Consensual.words
  coord.charac <- cbind.data.frame(res.consensualWords$Centroids[which(rownames(res.consensualWords$Centroids)%in%wordsCons), axis],
                                   Cluster = rep(as.character(nlevels(coord.stim$Cluster)+1), length(wordsCons)))
  colnames(coord.charac)[1:2] <- c("AxeA", "AxeB")
  coord.stim.charac <- rbind.data.frame(coord.stim, coord.charac)
  coord.stim.charac <- cbind.data.frame(coord.stim.charac, c(rep("stimulus", nrow(coord.stim)), rep("characteristic", nrow(coord.charac))))
  colnames(coord.stim.charac)[4] <- "Type"
  coord.stim.charac$Type <- relevel(coord.stim.charac$Type, ref = "stimulus")
  
  partition.stim <- as.integer(coord.stim$Cluster)
  names(partition.stim) <- rownames(coord.stim)
  res[[5]] <- partition.stim
    
  dta.charact.by.stim <- as.data.frame(matrix(NA, nbstim, 1))
  rownames(dta.charact.by.stim) <- rownames(dta.final)
  colnames(dta.charact.by.stim) <- "Description"
  for (i in 1 : nrow(dta.charact.by.stim)) {
    dta.charact.by.stim[i, 1] <- paste(unlist(strsplit(dta.final[i,], sep.charact)), collapse = " ; ")
  }
  dta.charact.by.stim <- merge(coord.stim, dta.charact.by.stim, by = "row.names")
  rownames(dta.charact.by.stim) <- dta.charact.by.stim[, 1]
  dta.charact.by.stim <- dta.charact.by.stim[, -(1 : 3)]
  res.textual <- textual(dta.charact.by.stim, num.text = 2, contingence.by = 1, sep.word = " ; ")
  res.textual$cont.table <- res.textual$cont.table[, -which(apply(res.textual$cont.table, 2, "sum") < nbtimes.consens.charact)]
  res.desc.clust.stim.text <- descfreq(res.textual$cont.table)
  names(res.desc.clust.stim.text) <- paste0("category", names(res.desc.clust.stim.text))
  if (!is.null(id.info.stim)) {
    dta.info.stim <- as.data.frame(dta.sauv[1 : nbstim, id.info.stim])
    colnames(dta.info.stim) <- colnames(dta.sauv)[id.info.stim]
    rownames(dta.info.stim) <- rownames(dta)
    dta.info.stim <- merge(coord.stim, dta.info.stim, by = "row.names")
    rownames(dta.info.stim) <- dta.info.stim[, 1]
    dta.info.stim <- dta.info.stim[, -(1 : 3)]
    for (i in 1 : length(type.info.stim)) {
      if (type.info.stim[i] == "cat") {
        dta.info.stim[, i + 1] <- as.factor(dta.info.stim[, i + 1])
      }
    }
    res.desc.clust.stim.sup <- catdes(dta.info.stim, num.var = which(colnames(dta.info.stim) == "Cluster"))
    res[[6]] <- list(res.desc.clust.stim.text, res.desc.clust.stim.sup)
    names(res[[6]]) <- c("textual.characteristics", "sup.info")
  } else {
    res[[6]] <- res.desc.clust.stim.text
  }
  
  # plot the stimuli representation
  palette.col <- c("#90B08F", "#EA485C", "#FF8379", "#009193", "#FFCEA5", "#A9A9A9", "#B0983D", "#941751", "#333333", "#A8D9FF")
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
  if ((Sys.getenv("RSTUDIO") == "1") == FALSE) {
    empty.dev <- (dev.cur() == 1)
  }
  legend.plot.stim <- get.legend(plot.legend.clust.stim)
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
  
  # define the remarkability of the stimuli
  compute.contingency <- function(dta) {
    conting <- matrix(NA, nbrater, nbrater)
    rownames(conting) <- colnames(conting) <- rownames(dta)
    for (i in 1 : ncol(conting)) {
      for (j in 1 : ncol(conting)) {
        stim.together <- which(dta[i,] == dta[j,])
        stim.na <- which(dta[i,] == "" | dta[j,] == "")
        conting[i, j] <- conting[j, i] <- length(which(stim.together %in% stim.na == FALSE))
      }
    }
    return(conting)
  }
  compute.remarkability <- function(step) {
    s <- step
    dta.step <- dta[, seq(s, ncol(dta), length(sast.parameters))]
    if (s == length(sast.parameters)) {
      res.mca.step <- MCA(dta.step , graph = FALSE)
      contrib <- apply(res.mca.step$ind$contrib[, 1 : 2], 1, "sum")
      vec.remark <- vector(length = nbstim)
      names(vec.remark) <- rownames(dta.step)
      vec.remark[which(contrib >= quantile(contrib, probs = 0.85))] <- "R"
      vec.remark[which(contrib < quantile(contrib, probs = 0.85))] <- "I"
    } else {
      dta.compt.step <- compute.contingency(dta.step)
      res.ca.step <- CA(dta.compt.step, graph = FALSE)
      contrib <- apply(res.ca.step$row$contrib[, 1 : 2], 1, "sum")
      vec.remark <- vector(length = nbstim)
      names(vec.remark) <- rownames(dta.step)
      vec.remark[which(contrib >= quantile(contrib, probs = 0.85))] <- "R"
      vec.remark[which(contrib < quantile(contrib, probs = 0.85))] <- "I"
    }
    return(cbind.data.frame(contrib, vec.remark))
  }
  dta.remark <- as.data.frame(matrix(NA, nbstim, 1))
  rownames(dta.remark) <- rownames(dta)
  for (s in 1 : length(sast.parameters)) {
    dta.remark <- cbind.data.frame(dta.remark, compute.remarkability(step = s))
  }
  colnames(dta.remark) <- c("type",
                            paste(rep(c("contrib", "type"),  length(sast.parameters)),  
                                  rep(paste0("step", 1 : length(sast.parameters)), each = 2), 
                                  sep = "_"))
  for (i in 1 : nrow(dta.remark)) {
    if("R" %in% as.character(as.factor(unlist(dta.remark[i, -c(1, seq(2, ncol(dta.remark), by = 2), ncol(dta.remark))])))) {
      dta.remark[i, "type"] <- "Remarkable"
    } else {
      dta.remark[i, "type"] <- "Insignificant"
    }
    if (as.character(as.factor(unlist(dta.remark[i, ncol(dta.remark)]))) == "R") {
      dta.remark[i, "type"] <- paste(dta.remark[i, "type"], "Remarkable", sep = " -> ")
    } else {
      dta.remark[i, "type"] <- paste(dta.remark[i, "type"], "Insignificant", sep = " -> ")
    }
  }
  res[[7]] <- dta.remark
  
  generate.plot.remark <- function(stim) {
    dta.remark.step <- cbind.data.frame(1 : length(sast.parameters),
                                        t(dta.remark[stim, seq(2, ncol(dta.remark), by = 2)]),
                                        t(dta.remark[stim, seq(3, ncol(dta.remark), by = 2)]))
    colnames(dta.remark.step) <- c("Step", "Contribution", "Type")
    if (levels(dta.remark.step$Type) == "R") {
      palette.col.remark <- palette.col[2]
    } else {
      palette.col.remark <- palette.col[1 : 2]
    }
    if ((dta.remark[stim, 1] == "Insignificant -> Remarkable") | (dta.remark[stim, 1] == "Remarkable -> Insignificant")) {
      col.background = "#E5E5E5"
    } else {
      col.background = "white"
    }
    plot.remark <- ggplot(NULL) +
      ylim(0, 100) +
      geom_line(data = dta.remark.step, aes(Step, Contribution), color = "#444444") +
      geom_point(data = dta.remark.step, aes(Step, Contribution, color = Type), size = 4) +
      geom_text(data = dta.remark.step, aes(x = Step, y = (Contribution + 6), label = Type, color = Type), size = 4) +
      scale_x_discrete(labels = paste("Step", 1 : length(sast.parameters)),
                       limits = 1 : 3) +
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
  plots.remark <- lapply(1 : nbstim, generate.plot.remark)
  plot.blank <- ggplot(data.frame()) + 
    geom_point() + 
    theme_minimal() + 
    theme(panel.background = element_rect(fill = 'transparent', colour = "transparent"))
  last.plot <- length(plots.remark)
  for (i in 1 : (nbstim - (floor(nbstim/4) * 4))) {
    plots.remark[[last.plot+i]] <- plot.blank
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
  
  # compute supplementary plots about the SAS task
  generate.plot.nb.groups.step <- function(step) {
    vec.group.final <- vector()
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
      scale_x_discrete(limits = 1 : nb.group.max) +
      theme(
        panel.background = element_rect(fill = 'white', colour = "#444444"),
        panel.grid.major = element_line(colour = "white"),
        panel.grid.minor = element_line(colour = "white"),
        axis.text = element_text(colour = "#444444", size = 10),
        axis.ticks = element_line(colour = "#444444"),
        axis.title = element_text(colour = "white"),
        plot.title = element_text(face = "bold", hjust = 0.5, size = 12, color = "#444444"),
        legend.position = "none")
    return(list(plot.nb.groups, nb.group.occ.max))
  }
  plots.nb.groups.step <- lapply(1 : length(sast.parameters), generate.plot.nb.groups.step)
  vec.nb.groups.occ.max <- vector()
  for (i in 1 : length(plots.nb.groups.step)) {
    vec.nb.groups.occ.max[i] <- plots.nb.groups.step[[i]][[2]]
  }
  last.plot <- length(plots.nb.groups.step)
  if ((length(sast.parameters) - (floor(length(sast.parameters)/3) * 3)) != 0) {
    for (i in 1 : (length(sast.parameters) - (floor(length(sast.parameters)/3) * 3))) {
      plots.nb.groups.step[[last.plot+i]] <- plot.blank
    }
  }
  if (graph == TRUE) {
    if ((Sys.getenv("RSTUDIO") == "1") == FALSE | ext.dev.Rstudio == TRUE) {
      dev.new(noRStudioGD = TRUE)
    }
    if (length(sast.parameters) != (floor(length(sast.parameters)/3) * 3)) {
      nb.wind <- floor(length(sast.parameters)/3) + 1
    } else {
      nb.wind <- floor(length(sast.parameters)/3)
    }
    for (i in 1 : nb.wind) {
      grid.arrange(plots.nb.groups.step[[3 * i - 2]][[1]] + ylim(0, max(vec.nb.groups.occ.max) + 2),
                   plots.nb.groups.step[[3 * i - 1]][[1]] + ylim(0, max(vec.nb.groups.occ.max) + 2),
                   plots.nb.groups.step[[3 * i]][[1]] + ylim(0, max(vec.nb.groups.occ.max) + 2),
                   nrow = 3, ncol = 1)
    }
  }
  
  generate.plot.nb.stim.group.step <- function(step) {
    vec.stim.final <- vector()
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
      scale_x_discrete(limits = 1 : nb.stim.max) +
      theme(
        panel.background = element_rect(fill = 'white', colour = "#444444"),
        panel.grid.major = element_line(colour = "white"),
        panel.grid.minor = element_line(colour = "white"),
        axis.text = element_text(colour = "#444444", size = 10),
        axis.ticks = element_line(colour = "#444444"),
        axis.title = element_text(colour = "white"),
        plot.title = element_text(face = "bold", hjust = 0.5, size = 12, color = "#444444"),
        legend.position = "none")
    return(list(plot.nb.stim, nb.stim.occ.max))
  }
  plots.nb.stim.step <- lapply(1 : length(sast.parameters), generate.plot.nb.stim.group.step)
  vec.nb.stim.occ.max <- vector()
  for (i in 1 : length(plots.nb.stim.step)) {
    vec.nb.stim.occ.max[i] <- plots.nb.stim.step[[i]][[2]]
  }
  last.plot <- length(plots.nb.stim.step)
  if ((length(sast.parameters) - (floor(length(sast.parameters)/3) * 3)) != 0) {
    for (i in 1 : (length(sast.parameters) - (floor(length(sast.parameters)/3) * 3))) {
      plots.nb.stim.step[[last.plot+i]] <- plot.blank
    }
  }
  if (graph == TRUE) {
    if ((Sys.getenv("RSTUDIO") == "1") == FALSE | ext.dev.Rstudio == TRUE) {
      dev.new(noRStudioGD = TRUE)
    }
    if (length(sast.parameters) != (floor(length(sast.parameters)/3) * 3)) {
      nb.wind <- floor(length(sast.parameters)/3) + 1
    } else {
      nb.wind <- floor(length(sast.parameters)/3)
    }
    for (i in 1 : nb.wind) {
      grid.arrange(plots.nb.stim.step[[3 * i - 2]][[1]] + ylim(0, max(vec.nb.stim.occ.max) + 2),
                   plots.nb.stim.step[[3 * i - 1]][[1]] + ylim(0, max(vec.nb.stim.occ.max) + 2),
                   plots.nb.stim.step[[3 * i]][[1]] + ylim(0, max(vec.nb.stim.occ.max) + 2),
                   nrow = 3, ncol = 1)
    }
  }
  
  generate.plot.nb.char.group.step <- function(step) {
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
      scale_x_discrete(limits = 1 : nb.char.max) +
      theme(
        panel.background = element_rect(fill = 'white', colour = "#444444"),
        panel.grid.major = element_line(colour = "white"),
        panel.grid.minor = element_line(colour = "white"),
        axis.text = element_text(colour = "#444444", size = 10),
        axis.ticks = element_line(colour = "#444444"),
        axis.title = element_text(colour = "white"),
        plot.title = element_text(face = "bold", hjust = 0.5, size = 12, color = "#444444"),
        legend.position = "none")
    return(list(plot.nb.char, nb.char.occ.max))
  }
  plots.nb.char.step <- lapply(1 : length(sast.parameters), generate.plot.nb.char.group.step)
  vec.nb.char.occ.max <- vector()
  for (i in 1 : length(plots.nb.char.step)) {
    vec.nb.char.occ.max[i] <- plots.nb.char.step[[i]][[2]]
  }
  last.plot <- length(plots.nb.char.step)
  if ((length(sast.parameters) - (floor(length(sast.parameters)/3) * 3)) != 0) {
    for (i in 1 : (length(sast.parameters) - (floor(length(sast.parameters)/3) * 3))) {
      plots.nb.char.step[[last.plot+i]] <- plot.blank
    }
  }
  if (graph == TRUE) {
    if ((Sys.getenv("RSTUDIO") == "1") == FALSE | ext.dev.Rstudio == TRUE) {
      dev.new(noRStudioGD = TRUE)
    }
    if (length(sast.parameters) != (floor(length(sast.parameters)/3) * 3)) {
      nb.wind <- floor(length(sast.parameters)/3) + 1
    } else {
      nb.wind <- floor(length(sast.parameters)/3)
    }
    for (i in 1 : nb.wind) {
      grid.arrange(plots.nb.char.step[[3 * i - 2]][[1]] + ylim(0, max(vec.nb.char.occ.max) + 2),
                   plots.nb.char.step[[3 * i - 1]][[1]] + ylim(0, max(vec.nb.char.occ.max) + 2),
                   plots.nb.char.step[[3 * i]][[1]] + ylim(0, max(vec.nb.char.occ.max) + 2),
                   nrow = 3, ncol = 1)
    }
  }
  
  generate.plot.nb.cit.char.step <- function(step) {
    s <- step
    dta.step <- dta[, seq(s, ncol(dta), length(sast.parameters))]
    if (s == length(sast.parameters)) {
      dta.step <- as.vector(dta.step)
    } else {
      dta.step <- as.vector(dta.step)[-which(as.vector(dta.step) == "")]
    }
    vec.cit.char.step <- table(unlist(strsplit(dta.step, sep.charact)))
    vec.cit.char.step <- sort(vec.cit.char.step, decreasing = FALSE)
    if (length(vec.cit.char.step) > 15) {
      vec.cit.char.step <- vec.cit.char.step[1 : 15]
    }
    dta.nb.cit.char <- as.data.frame(vec.cit.char.step)
    colnames(dta.nb.cit.char) <- c("Characteristic", "Nb.cit.char")
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
    return(list(plot.nb.cit.char, nb.cit.char.occ.max))
  }
  plots.nb.cit.char.step <- lapply(1 : length(sast.parameters), generate.plot.nb.cit.char.step)
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
  
  # segment the panel of participants
  sim.mat.part <- as.data.frame(matrix(NA, nbrater, nbrater))
  rownames(sim.mat.part) <- colnames(sim.mat.part) <- gsub(paste0(sep.part.step, ".*$"), "", colnames(dta.final))
  for (i in 1 : nbrater) {
    for (j in 1 : nbrater) {
      if (is.na(sim.mat.part[i, j])) {
        sim.mat.part[i, j] <- RandIndex(dta.final[,i], dta.final[,j])$AdjustedRand
      }
    }
  }
  res[[8]] <- sim.mat.part
  dissim.mat.part <- as.dist(1 - sim.mat.part)
  dendrogram <- stats::hclust(dissim.mat.part, method = "ward.D2")
  nb.clust.part <- NbClust(diss = dissim.mat.part, distance = NULL, method = "ward.D2", index = "mcclain",  min.nc = 2, max.nc = 6)$Best.nc[1]
  partition <- cutree(dendrogram, k = nb.clust.part)
  mat.partition <- cbind.data.frame(partition, names(partition))
  colnames(mat.partition) <- c("Cluster", "Rater")
  res[[9]] <- partition
  
  palette.col <- c("#90B08F", "#EA485C", "#FF8379", "#009193", "#FFCEA5", "#A9A9A9", "#B0983D", "#941751", "#333333", "#A8D9FF")
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
  res[[10]] <- list(data.segments, data.labels, dendrogram)
  names(res[[10]]) <- c("data.segments", "data.labels", "dendrogram")
  
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
  legend.plot.part <- get.legend(plot.legend.clust.part)
  if (graph == TRUE) {
    if ((Sys.getenv("RSTUDIO") == "1") == FALSE | ext.dev.Rstudio == TRUE) {
      dev.new(noRStudioGD = TRUE)
    }
    grid.arrange(arrangeGrob(plot.dendro + theme(legend.position="none"),
                             ncol = 1, nrow = 1),
                 legend.plot.part, nrow = 2, heights = c(8, 1))
  }
  
  consensus.partition.stim <- list()
  for (i in 1 : nlevels(as.factor(partition))) {
    consensus.partition.stim[[i]] <- list()
    dta.clust.sauv <- dta.final[, which(colnames(sim.mat.part) %in% names(which(partition == i)))]
    dta.clust <- SortingPartition(dta.clust.sauv)
    consens.part <- ConsensusPartition(dta.clust)$Consensus
    consensus.partition.stim[[i]][[1]] <- consens.part
    dta.charact.by.stim.clust <- as.data.frame(matrix(NA, nbstim, 1))
    rownames(dta.charact.by.stim.clust) <- rownames(dta.final)
    colnames(dta.charact.by.stim.clust) <- "Description"
    for (j in 1 : nrow(dta.charact.by.stim.clust)) {
      dta.charact.by.stim.clust[j, 1] <- paste(unlist(strsplit(dta.clust.sauv[j,], sep.charact)), collapse = " ")
    }
    consens.part <- as.data.frame(consens.part)
    colnames(consens.part) <- "Cluster"
    dta.charact.by.stim.clust <- merge(consens.part, dta.charact.by.stim.clust, by = "row.names")
    rownames(dta.charact.by.stim.clust) <- dta.charact.by.stim.clust[, 1]
    dta.charact.by.stim.clust <- dta.charact.by.stim.clust[, -1]
    res.textual.clust <- textual(dta.charact.by.stim.clust, num.text = 2, contingence.by = 1)
    res.desc.clust.stim.clust.text <- descfreq(res.textual.clust$cont.table)
    names(res.desc.clust.stim.clust.text) <- paste0("category.stim", names(res.desc.clust.stim.clust.text))
    consensus.partition.stim[[i]][[2]] <- res.desc.clust.stim.clust.text
    names(consensus.partition.stim[[i]]) <- c("partition", "characterisation")
  }
  names(consensus.partition.stim) <- paste0("category", 1 : length(consensus.partition.stim))
  res[[11]] <- consensus.partition.stim
  
  if (!is.null(id.info.part)) {
    dta.info.part <- dta.sauv[id.info.part,]
    if (!is.null(id.info.stim)) {
      dta.info.part <- dta.info.part[, -id.info.stim]
    }
    dta.info.part <- t(dta.info.part[, seq(1, ncol(dta.info.part), by = length(sast.parameters))])
    rownames(dta.info.part) <- gsub(paste0(sep.part.step, ".*$"), "", rownames(dta.info.part))
    dta.info.part <- merge(dta.info.part, partition, by = "row.names")
    colnames(dta.info.part)[ncol(dta.info.part)] <- "Cluster"
    dta.info.part$Cluster <- as.factor(dta.info.part$Cluster)
    rownames(dta.info.part) <- dta.info.part[, 1]
    dta.info.part <- dta.info.part[, -1]
    for (i in 1 : length(type.info.part)) {
      if (type.info.part[i] == "cat") {
        dta.info.part[, i] <- as.factor(dta.info.part[, i])
      }
    }
    res.desc.clust.part <- catdes(dta.info.part, num.var = which(colnames(dta.info.part) == "Cluster"))
    res[[12]] <- res.desc.clust.part
  } else {
    res[12] <- list(NULL)
  }
  
  # return the results
  names(res) <- c("call", "res.mca", "coocc.stim", "consensual.charact.stim", "partition.stim", "charact.clust.stim", "remark.stim", "ARI.part", "partition.part", "res.plot.dendro.part", "cons.partition.stim.clust", "charact.clust.part")
  message("Analysis performed")
  options(warn = 0)
  class(res) <- c("SAStask", "list ")
  
}