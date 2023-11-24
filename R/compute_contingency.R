compute_contingency <- function(
  dta
  ) {

  nbstim <- nrow(dta)
  conting <- matrix(NA, nbstim, nbstim)
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
