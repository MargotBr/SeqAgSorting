get_stim_remarkability <- function(
  nbstim,
  dta,
  sast.parameters
  ) {

  dta.remark <- as.data.frame(matrix(NA, nbstim, 1))

  rownames(dta.remark) <- rownames(dta)

  for (s in 1 : length(sast.parameters)) {
    dta.remark <- cbind.data.frame(
      dta.remark,
      compute_remarkability_one_step(
        step = s,
        dta = dta,
        sast.parameters = sast.parameters
      )
    )
  }

  colnames(dta.remark) <- c("type",
                            paste(
                              rep(
                                c("contrib", "type"),
                                length(sast.parameters)
                              ),
                              rep(
                                paste0(
                                  "step",
                                  1 : length(sast.parameters)
                                ),
                                each = 2
                              ),
                              sep = "_")
                            )

  for (i in 1 : nrow(dta.remark)) {

    if ("R" %in% as.character(
      as.factor(unlist(dta.remark[i, -c(1, seq(2, ncol(dta.remark), by = 2), ncol(dta.remark))])))
      ) {
      dta.remark[i, "type"] <- "Remarkable"
    } else {
      dta.remark[i, "type"] <- "Insignificant"
    }

    if (as.character(
      as.factor(unlist(dta.remark[i, ncol(dta.remark)]))) == "R"
      ) {
      dta.remark[i, "type"] <- paste(dta.remark[i, "type"], "Remarkable", sep = " -> ")
    } else {
      dta.remark[i, "type"] <- paste(dta.remark[i, "type"], "Insignificant", sep = " -> ")
    }
  }

  return(dta.remark)

}
