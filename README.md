The `SAStask` package is dedicated to the statistical analysis of Sequential Agglomerative Sorting task data. It first analyses data trough a stimulus-oriented approach. In this context, it considers a Multiple Correspondance Analysis performed on the final partitions of the stimuli provided by the participants. This procedure provides a multidimensional representation of the stimuli. On this representation, two stimuli are close (resp. distant) if they have been perceived as sensorially similar (resp. different) by the participants. The SAStask package then analyses data trough a participant-oriented approach. In this context, the structure of disagreement among the panel of participants is studied through the computation of a dissimilarity index. This index corresponds to 1 - ARI, ARI being the Adjusted Rand Index between the partitions of the stimuli provided by the two corresponding participants. Finally, this participant-oriented approach provides a segmentation of the participants, each cluster of participants being assimilated to a pattern of perception of stimuli.

# <span style="color: #EA485C">Installing SAStask</span>

To get the current development version from GitHub:

  ```{r eval=FALSE}
install.packages("devtools")
devtools::install_github("MargotBr/SAStask", build_vignettes = FALSE)
library(SAStask)
```

# <span style="color: #EA485C">Using SAStask</span>

To get an overview of the functionalities of the package, read the corresponding vignette:

  ```{r eval=FALSE}
vignette("BinQmet")
```
