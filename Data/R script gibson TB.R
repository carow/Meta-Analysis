#setwd("C:/Users/ICI/Desktop/MetaAnalysisD")
setwd("/Users/Torfinn/Documents/Uni Freiburg/Best Practice R/Meta-Analysis/Data")

table1 <- read.csv2("Gibson.csv")
str(table1)
summary(table1)

# To make the dataset readable for R
table1$d.mean=as.numeric(table1$d.mean)
table1$p.mean=as.numeric(table1$p.mean)
table1$p.sd=as.numeric(table1$p.sd)
table1$d.sd=as.numeric(table1$d.sd)
table1$hedges.g.=as.numeric(table1$hedges.g)

# To ommit columns that we don't need in the dataset
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
rma.he = rma(method="HE", m1i = p.mean, sd1i = p.sd, n1i = p.n, 
             m2i = d.mean, sd2i = d.sd, n2i =d.n, measure = "SMD", 
             data=birdsnew)
rma.he
detach(birdsnew)


# Plot for study contributions to the effect size
contributions <- 1/rma.he$vi/sum(1/rma.he$vi) * 100
cbind(contributions)
barplot(contributions)

#Get Between-Study Variance & Its Error
rma.he$tau2 + 1.96 * c(-1, 1) * rma.he$se.tau2 #95% CI

# I2, mean differences
# Interpretation = Percentage of "unexplained" variance
I2 <- with(rma.he, (QE - (k - 1))/QE * 100) 
I2

# Thresholds For I 2
# 0% to 30% → Low
# 30% to 60% → Moderate
# 50% to 90% → Substantial
# 75% to 100% → Considerable

# Forest plot for our model:

forest.rma(rma.he, showweight=T) # DEFAULT PLOT

#Funnel plots for our model
funnel(rma.he)

attach(birds)


detach(birds)
attach(birdsnew)
res <- rma(yi=g, vi=var.g, method="DL")
res
detach(birdsnew)

attach(effectsizes)
value <- fsn(y = g, v = var.g)
value
detach(effectsizes)

attach(effectsizes)
res.rd <- rma(yi=g, vi=var.g, method="DL",
                 measure = "RD",) # Risk Differences
trimfill(res.rd) # Only applicable for FE or RE objects
funnel(trimfill(res.rd))
detach(effectsizes)

# Old escalc function for doing the effect size measures
#attach(birds)
#es = escalc(measure = "SMD", m1i = d.mean, sd1i = d.sd, n1i = d.n, m2i = p.mean, sd2i = p.sd, n2i = p.n, data=birds)
#detach(birds)
#attach(es)
#result.he = rma(yi=yi, vi=vi, method ="HE")
#result.he
#summary(result.he)
#detach(es)



#library(compute.es)
#mes2 from the package compute.es
#SDpooled = sqrt(((p.n-1)*p.sd^2+(d.n-1)*d.sd^2)/(p.n+d.n-2))

#str(SDpooled)
#effectsizes=mes2(m.1=p.mean, m.2= d.mean,s.pooled=SDpooled, n.1=p.n, n.2=d.n)


