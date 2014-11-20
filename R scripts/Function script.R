setwd("/Users/Torfinn/Documents/Uni Freiburg/Best Practice R/Meta-Analysis/R scripts")
file.exists("/Users/Torfinn/Documents/Uni Freiburg/Best Practice R/Meta-Analysis/R scripts/rma.RE")
load("/Users/Torfinn/Documents/Uni Freiburg/Best Practice R/Meta-Analysis/R scripts/rma.RE")

library(metafor)
library(meta)

forest.rma(rma.RE, annotate = TRUE, cex = 0.5, showweight = TRUE) #RE model
