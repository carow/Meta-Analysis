#setwd("C:/Users/ICI/Desktop/MetaAnalysisD")
setwd("/Users/Torfinn/Documents/Uni Freiburg/Best Practice R/Meta-Analysis/Data")

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
es = escalc(measure = "SMD", m1i = d.mean, sd1i = d.sd, n1i = d.n, m2i = p.mean, sd2i = p.sd, n2i = p.n, data=birds)

detach(birds)

attach(birds)
rma.he = rma(method="HE", m1i = d.mean, sd1i = d.sd, n1i = d.n, m2i = p.mean, sd2i = p.sd, n2i = p.n, measure = "SMD", data=birds)
print(rma.he)
detach(birds)

attach(es)
result.he = rma(yi=yi, vi=vi, method ="HE")
result.he
summary(result.he)
detach(es)

#Plot for study contributions to the effect size
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

forest.rma(rma.he, order = "prec", showweight=T) # DEFAULT PLOT

attach(birds)

library(compute.es)
#mes2 from the package compute.es
SDpooled = sqrt(((p.n-1)*p.sd^2+(d.n-1)*d.sd^2)/(p.n+d.n-2))


str(SDpooled)
effectsizes=mes2(m.1=p.mean, m.2= d.mean,s.pooled=SDpooled, n.1=p.n, n.2=d.n)

detach(birds)
attach(effectsizes)
res <- rma(yi=g, vi=var.g, method="DL")
res
detach(effectsizes)

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
