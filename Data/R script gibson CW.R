
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

#to make 
library(data.table)
DT <- as.data.table(birds)           # convert to data.table
setkey(DT, study.ID)                    # set key to allow binary search using `J()`
birdsnew = DT[J(unique(study.ID)), mult ='first']  # if you wanted the first row for each x



attach(birdsnew)

#Fixed Effect Model

rma.FE = rma(method = "FE", measure = "SMD", m1i = p.mean, m2i = d.mean, sd1i = p.sd, sd2i = d.sd, n1i = p.n, n2i = d.n, vtype = "UB")
rma.FE


#Random Effects Model
rma.RE = rma(method = "REML", measure = "SMD", m1i = p.mean, m2i = d.mean, sd1i = p.sd, sd2i = d.sd, n1i = p.n, n2i = d.n, vtype = "UB")
rma.RE

detach(birdsnew)


forest.rma (rma.FE, annotate = TRUE, cex = 0.5) #FE model

forest.rma(rma.RE, annotate = TRUE, cex = 0.5) #RE model


#Causes of heterogeneity - Meta-regression
#with the mods argument in the rma?? p. 141

attach(birdsnew)

rma.FE = rma(method = "FE", measure = "SMD", m1i = p.mean, m2i = d.mean, sd1i = p.sd, sd2i = d.sd, n1i = p.n, n2i = d.n, vtype = "UB", mods = ~ continent)
rma.FE

#Random Effects Model
rma.RE = rma(method = "REML", measure = "SMD", m1i = p.mean, m2i = d.mean, sd1i = p.sd, sd2i = d.sd, n1i = p.n, n2i = d.n, vtype = "UB", mods = ,continent)
rma.RE

detach(birdsnew)


#publication bias testing
funnel(rma.FE)
regtest(rma.FE)#analysing the asymmetry of the funnel plot

funnel(rma.RE)
regtest(rma.RE, model = "rma", predictor = "sei")


#Fail safe n method
fsn(yi = rma.FE$yi, vi = rma.FE$vi)#"file drawer analysis"

fsn(yi = rma.RE$yi, vi = rma.RE$vi)



#sensitivity analysis/robustness testing
#with the leaveout function


leave1out(rma.FE)

sens.RE = leave1out(rma.RE)
sens.RE








#Old calculations
attach(birdsnew)

library(compute.es)
#mes2 from the package compute.es
SDpooled = sqrt(((p.n-1)*p.sd^2+(d.n-1)*d.sd^2)/(p.n+d.n-2))


str(SDpooled)
effectsizes=mes2(m.1=p.mean, m.2= d.mean,s.pooled=SDpooled, n.1=p.n, n.2=d.n)

detach(birdsnew)

attach(effectsizes)
res1 = rma(yi = g, vi = var.g, method = "DL")
res

detach(effectsizes)

res1 = rma(yi =effectsizes$g, vi = effectsizes$var.g, method = "DL",  slab=paste(birds$studyID))

forest.rma(res1, annotate=TRUE, cex=0.5 )