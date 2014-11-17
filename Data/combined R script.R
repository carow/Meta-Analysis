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

# To select randomly one row per study to narrow down the metaanalysis.
library(devtools)
source_gist("https://gist.github.com/mrdwab/6424112")
set.seed(100) 
birdsnew = stratified(birds, "study.ID", 1)
summary(birdsnew)


attach(birdsnew)

#Fixed Effect Model

rma.FE = rma(method = "FE", measure = "SMD", m1i = p.mean, m2i = d.mean, sd1i = p.sd, sd2i = d.sd, n1i = p.n, n2i = d.n, vtype = "UB")
rma.FE


#Random Effects Model
rma.RE = rma(method = "REML", measure = "SMD", m1i = p.mean, m2i = d.mean, sd1i = p.sd, sd2i = d.sd, n1i = p.n, n2i = d.n, vtype = "UB")# maybe change vtype and method
rma.RE

detach(birdsnew)


forest.rma (rma.FE, annotate = TRUE, cex = 0.5, showweight = TRUE) #FE model

forest.rma(rma.RE, annotate = TRUE, cex = 0.5, showweight = TRUE) #RE model


#Causes of heterogeneity - Meta-regression p. 141

attach(birdsnew)

rma.FE.meta = rma(method = "FE", measure = "SMD", m1i = p.mean, m2i = d.mean, sd1i = p.sd, sd2i = d.sd, n1i = p.n, n2i = d.n, vtype = "UB", mods = ~ continent)
rma.FE.meta

#Random Effects Model
rma.RE.meta = rma(method = "REML", measure = "SMD", m1i = p.mean, m2i = d.mean, sd1i = p.sd, sd2i = d.sd, n1i = p.n, n2i = d.n, vtype = "UB", mods = ~ continent)
rma.RE.meta

detach(birdsnew)


##publication bias testing
funnel(rma.FE)
regtest(rma.FE)#analysing the asymmetry of the funnel plot

funnel(rma.RE)
regtest(rma.RE, model = "rma", predictor = "sei")


#-- Fail safe n method
fsn(yi = rma.FE$yi, vi = rma.FE$vi)#"file drawer analysis"

fsn(yi = rma.RE$yi, vi = rma.RE$vi)


# -- Trim fill method
attach(birdsnew)
rma.TF <- rma( method="DL", measure = "RD",m1i = p.mean, m2i = d.mean, sd1i = p.sd, sd2i = d.sd, n1i = p.n, n2i = d.n) # Risk Differences

trimfill(rma.RE) # Only applicable for FE or RE objects
funnel(trimfill(rma.RE))

detach(birdsnew)


#sensitivity analysis/robustness testing
#-- with the leaveout function


leave1out(rma.FE)

sens.RE = leave1out(rma.RE)
sens.RE




