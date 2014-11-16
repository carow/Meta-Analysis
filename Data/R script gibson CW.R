setwd("C:/Users/ICI/Desktop/MetaAnalysisD")
library(metafor)
table1 <- read.csv2("Gibson.csv")
str(table1)
summary(table1)

table1$d.mean=as.numeric(table1$d.mean)
table1$p.mean=as.numeric(table1$p.mean)
table1$p.sd=as.numeric(table1$p.sd)
table1$d.sd=as.numeric(table1$d.sd)
table1$hedges.g.=as.numeric(table1$hedges.g)


attach(table1)
birds=table1[taxon=="b",-c(5,6,7,8,9,10,11,12,13)]
detach(table1)



attach(birds)

library(compute.es)
#mes2 from the package compute.es
SDpooled = sqrt(((p.n-1)*p.sd^2+(d.n-1)*d.sd^2)/(p.n+d.n-2))


str(SDpooled)
effectsizes=mes2(m.1=p.mean, m.2= d.mean,s.pooled=SDpooled, n.1=p.n, n.2=d.n)

detach(birds)

attach(effectsizes)

res1 = rma(yi = g, vi = var.g, method = "DL")
res

detach(effectsizes)

res1 = rma(yi =effectsizes$g, vi = effectsizes$var.g, method = "DL",  slab=paste(birds$studyID))

forest.rma(res1, annotate=TRUE, cex=0.5 )




#publication bias testing
funnel(res)
regtest(res)


fsn(yi=g, vi=var.g, data=effectsizes)#By default the Rosentahl Approach "file drawer analysis"

#robustness testing