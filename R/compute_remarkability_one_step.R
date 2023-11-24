compute_remarkability_one_step <- function(
  dta,
  sast.parameters,
  step
  ) {

  s <- step

  nbstim <- nrow(dta)
  dta.step <- dta[, seq(s, ncol(dta), length(sast.parameters))]

  if (s == length(sast.parameters)) {
    res.mca.step <- MCA(dta.step , graph = FALSE)
    contrib <- apply(res.mca.step$ind$contrib[, 1 : 2], 1, "sum")
    vec.remark <- vector(length = nbstim)
    names(vec.remark) <- rownames(dta.step)
    vec.remark[which(contrib >= quantile(contrib, probs = 0.85))] <- "R"
    vec.remark[which(contrib < quantile(contrib, probs = 0.85))] <- "I"
  } else {
    dta.compt.step <- compute_contingency(dta.step)
    dta.pres.step <- compute_nb_pres(dta.step)
    dta.compt.pond.step <- dta.compt.step / dta.pres.step
    dta.compt.pond.step[which(is.na(dta.compt.pond.step))] <- 0
    res.ca.step <- CA(dta.compt.pond.step, graph = FALSE)
    contrib <- apply(res.ca.step$row$contrib[, 1 : 2], 1, "sum")
    vec.remark <- vector(length = nbstim)
    names(vec.remark) <- rownames(dta.step)
    vec.remark[which(contrib >= quantile(contrib, probs = 0.85))] <- "R"
    vec.remark[which(contrib < quantile(contrib, probs = 0.85))] <- "I"
  }

  return(
    cbind.data.frame(
      contrib,
      vec.remark
    )
  )

}
