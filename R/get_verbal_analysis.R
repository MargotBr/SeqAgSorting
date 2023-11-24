get_verbal_analysis <- function(
  res.fast,
  dta.final,
  nbtimes.consens.charact,
  proba.consens.charact,
  sep.charact,
  id.info.stim,
  dta.sauv,
  dta,
  type.info.stim,
  coord.stim,
  axis
  ){

  nbstim <- nrow(dta.final)

  res.consensualWords <- ConsensualWords(
    res.fast,
    nbtimes = nbtimes.consens.charact,
    proba = proba.consens.charact,
    graph = FALSE
  )

  wordsCons <- res.consensualWords$Consensual.words

  coord.charac <- cbind.data.frame(
    res.consensualWords$Centroids[which(rownames(res.consensualWords$Centroids)%in%wordsCons), axis],
    Cluster = rep(as.character(nlevels(coord.stim$Cluster)+1), length(wordsCons))
  )
  colnames(coord.charac)[1:2] <- c("AxeA", "AxeB")

  coord.stim.charac <- rbind.data.frame(
    coord.stim,
    coord.charac
  )

  coord.stim.charac <- cbind.data.frame(
    coord.stim.charac,
    c(
      rep("stimulus", nrow(coord.stim)),
      rep("characteristic", nrow(coord.charac))
    )
  )
  colnames(coord.stim.charac)[4] <- "Type"

  coord.stim.charac$Type <- as.factor(coord.stim.charac$Type)
  coord.stim.charac$Type <- relevel(coord.stim.charac$Type, ref = "stimulus")

  dta.charact.by.stim <- as.data.frame(matrix(NA, nbstim, 1))
  rownames(dta.charact.by.stim) <- rownames(dta.final)
  colnames(dta.charact.by.stim) <- "Description"

  for (i in 1 : nrow(dta.charact.by.stim)) {
    dta.charact.by.stim[i, 1] <- paste(
      unlist(strsplit(dta.final[i,], sep.charact)),
      collapse = " ; "
    )
  }

  dta.charact.by.stim <- merge(
    coord.stim,
    dta.charact.by.stim,
    by = "row.names"
  )
  rownames(dta.charact.by.stim) <- dta.charact.by.stim[, 1]
  dta.charact.by.stim <- dta.charact.by.stim[, -(1 : 3)]

  res.textual <- textual(
    dta.charact.by.stim,
    num.text = 2,
    contingence.by = 1,
    sep.word = " ; "
  )

  res.textual$cont.table <- res.textual$cont.table[, -which(apply(res.textual$cont.table, 2, "sum") < nbtimes.consens.charact)]

  res.desc.clust.stim.text <- descfreq(
    res.textual$cont.table
  )

  names(res.desc.clust.stim.text) <- paste0(
    "category",
    names(res.desc.clust.stim.text)
  )

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
    desc.clust.stim <- list(
      textual.characteristics = res.desc.clust.stim.text,
      sup.info = res.desc.clust.stim.sup
    )
  } else {
    desc.clust.stim<- res.desc.clust.stim.text
  }

  return(
    list(
      res.consensualWords = res.consensualWords,
      coord.stim.charac = coord.stim.charac,
      desc.clust.stim = desc.clust.stim
    )
  )

}
