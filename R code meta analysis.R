library(metafor)
data("dat.bcg")
print(dat.bcg, row.names = FALSE)

summary("dat.bcg")

res<-rma(measure="RR", ai=tpos, bi=tneg, ci=cpos, di=cneg, data=dat.bcg, method="DL", slab=paste(author, year, sep=", "))
res

#DL method is according to p. 1991 metafor, for inverse variance weights.

forest(res)

#indicative for publication bias
funnel(res)

#test for funnel plot assymetry
ranktest(res)
regtest(res)

#Fail safe n for publication bias
dat <- escalc(measure="RR", ai=tpos, bi=tneg, ci=cpos, di=cneg, data=dat.bcg)

fsn(yi, vi, data=dat) #By default the Rosentahl Approach "file drawer analysis"


#sensitivity analysis/robustness test

