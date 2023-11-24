compute_nb_pres <- function(
  dta
  ){

  nbstim <- nrow(dta)
  present <- matrix(NA, nbstim, nbstim)
  rownames(present) <- colnames(present) <- rownames(dta)
  for (i in 1:ncol(present)) {
    for (j in 1:ncol(present)) {
      pres.pdt1 <- which(dta[i, ] != "")
      pres.pdt2 <- which(dta[j, ] != "")
      present[i, j] <-
        present[j, i] <- length(which(duplicated(c(
          pres.pdt1, pres.pdt2
        ))))
    }
  }

  return(present)

}
