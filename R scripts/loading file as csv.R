library(metafor)

setwd("C:/Users/ICI/Documents/Carolina/Uni/Freiburg/3_Semester/Best practice R/Meta-Analysis/R scripts")
#setwd("/Users/Torfinn/Documents/Uni Freiburg/Best Practice R/Meta-Analysis/Data")
table1 <- read.csv2("Gibson3.csv")
str(table1)
summary(table1) 
head(table1)

table1$d.mean=as.numeric(table1$d.mean)
table1$p.mean=as.numeric(table1$p.mean)
table1$p.sd=as.numeric(table1$p.sd)
table1$d.sd=as.numeric(table1$d.sd)
table1$hedges.g.=as.numeric(table1$hedges.g)


attach(table1)
birds=table1[taxon=="b",-c(5,6,7,8,9,10,11,12,13)]
detach(table1)


# To select randomly one row per study to narrow down the metaanalysis.
library(devtools)
source_gist("https://gist.github.com/mrdwab/6424112")
set.seed(100) 
birdsnew = stratified(birds, "study.ID", 1)
summary(birdsnew)


attach(birdsnew)

