get_coocurence_matrix <- function(
  dta.final,
  sep.charact
  ){

  sink(file = "undesired message")
  res.fast <- invisible(
    fast(
      dta.final,
      sep.words = sep.charact,
      graph = FALSE
      )
  )
  sink()
  file.remove("undesired message")

  return(res.fast)

}
