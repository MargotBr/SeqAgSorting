
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Overview

The `SeqAgSorting` package is dedicated to the statistical analysis of
**Sequential Agglomerative Sorting task** data.

It first analyses data trough a **stimulus-oriented approach**. In this
context, it considers a Multiple Correspondance Analysis performed on
the final partitions of the stimuli provided by the participants. This
procedure provides a multidimensional representation of the stimuli. On
this representation, two stimuli are close (resp. distant) if they have
been perceived as sensorially similar (resp. different) by the
participants.

The SeqAgSorting package then analyses data trough a
**participant-oriented approach**. In this context, the structure of
disagreement among the panel of participants is studied through the
computation of a dissimilarity index. This index corresponds to 1 - ARI,
ARI being the Adjusted Rand Index between the partitions of the stimuli
provided by the two corresponding participants. Finally, this
participant-oriented approach provides a segmentation of the
participants, each cluster of participants being assimilated to a
pattern of perception of stimuli.

# Installation

To get the current development version from GitHub:

``` r
# install.packages("devtools")
devtools::install_github("MargotBr/SeqAgSorting", build_vignettes = FALSE)
library(SeqAgSorting)
```

# Usage

Use the `AnalyseSAS()` function to analyse the data and provide the
outputs:

``` r
# Read the pedagogic data set
data("pedagdata")

# Perform the statistical analysis
res.pedag <- analyse_sas(
  dta = pedagdata, 
  sast.parameters = c(4, 3, 3), 
  id.info.stim = 31, 
  type.info.stim = "cat", 
  id.info.part = 11 : 12, 
  type.info.part = rep("cat", 2), 
  nbtimes.consens.charact = 2, 
  proba.consens.charact = 0.10, 
  graph = FALSE
)

res.pedag
```
