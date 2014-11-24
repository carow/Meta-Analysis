library(metafor)

#setwd("C:/Users/ICI/Documents/Carolina/Uni/Freiburg/3_Semester/Best practice R/Meta Analysis/R scripts")
setwd("/Users/Torfinn/Documents/Uni Freiburg/Best Practice R/Meta-Analysis/R scripts")

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
data.sub = stratified(birds, "study.ID", 1)
summary(data.sub)


attach(data.sub)

# Our rma analysis
#Fixed Effect Model

rma.FE = rma(method = "FE", measure = "SMD", m1i = p.mean, m2i = d.mean, sd1i = p.sd, sd2i = d.sd, n1i = p.n, n2i = d.n)
rma.FE

#Random Effects Model
rma.RE = rma(method = "REML", measure = "SMD", m1i = p.mean, m2i = d.mean, sd1i = p.sd, sd2i = d.sd, n1i = p.n, n2i = d.n, vtype = "UB")# maybe change vtype and method
rma.RE

#http://www.google.de/url?sa=t&rct=j&q=&esrc=s&source=web&cd=5&cad=rja&uact=8&ved=0CEIQFjAE&url=http%3A%2F%2Fwww.researchgate.net%2Fprofile%2FEdward_Purssell%2Fpublication%2F262923251_Meta-analysis_using_metafor_in_R%2Flinks%2F00b7d5395634a8cf6f000000&ei=RWxsVIutCI2tPIHBgZgM&usg=AFQjCNHp1LAEr_ZSwhc9v7Vou-6LdFBlmQ&bvm=bv.80120444,d.ZWU
#


detach(data.sub)

#Dataset should be named data.sub
#rma of random effects should be called rma.RE, rma of fixed effects should be
#called rma.FE. IF a meta-regression has been conducted, it should be called
#rma.RE.meta or rma.FE.meta respectively. 
#The metafor package needs to be installed

# Automatisation should start here! Depending on size of dataset, change 
# cex = 

#if inherits class meta, if inherits class metafor.


# Create a table with the values from the rma they used. 
# Include ES, SE, CI, pval of ES, Q, I^2, Egger value for certain rma (fe/re),
# fail-safe n


#Create the forest plot as a figure. If FE used, then only FE plot. If RE,
#then only RE plot, if both, then both plots shown, maybe next to each other.

par(mar=c(5,3,2,2)) # for changing the size of the plot
#Forest plot for the FE model
forest.rma (rma.FE, cex = 0.5, showweight = TRUE) 

#Forest plot for the RE model
forest.rma(rma.RE, annotate = TRUE, cex = 0.5, showweight = TRUE) #RE model
par(mar=c(5,4,4,2)) # back to default


# check at ?par
# to split plotting window
# par(mfrow=c(2,2))
# to adjust margins



#Causes of heterogeneity - Meta-regression p. 141
# If they did their own meta-regression, create a table with the values 
# obtained by the meta-regression. Include the intercept and the other moderators


attach(data.sub)

rma.FE.meta = rma(method = "FE", measure = "SMD", m1i = p.mean, m2i = d.mean, sd1i = p.sd, sd2i = d.sd, n1i = p.n, n2i = d.n, vtype = "UB", mods = ~ continent)
rma.FE.meta


#Random Effects Model
rma.RE.meta = rma(method = "REML", measure = "SMD", m1i = p.mean, m2i = d.mean, sd1i = p.sd, sd2i = d.sd, n1i = p.n, n2i = d.n, vtype = "UB", mods = ~ continent)
rma.RE.meta

save(rma.RE.meta, file = "SweaveTorfinn")


detach(data.sub)


##publication bias testing
funnel(rma.FE)
regtest(rma.FE)#analysing the asymmetry of the funnel plot. Egger's test

funnel(rma.RE)
regtest(rma.RE, model = "rma", predictor = "sei")
#do it before or after accounting for a moderator??


#-- Fail safe n method
fsn(yi = rma.FE$yi, vi = rma.FE$vi) #"file drawer analysis"

fsn(yi = rma.RE$yi, vi = rma.RE$vi)


# -- Trim fill method
# Include a figure with the trim fill funnel?

#Fixed effects model
rma.TF.FE <- rma(method="FE", measure = "SMD", m1i = p.mean, m2i = d.mean, sd1i = p.sd, sd2i = d.sd, n1i = p.n, n2i = d.n) # Risk Differences
trimfill(rma.TF.FE) # Only applicable for FE or RE objects
funnel(trimfill(rma.TF.FE))

#Higgins 'E model
rma.TF.RE <-  rma(method="HE", measure = "SMD", m1i = p.mean, m2i = d.mean, sd1i = p.sd, sd2i = d.sd, n1i = p.n, n2i = d.n) # Risk Differences
trimfill(rma.TF.RE) # Only applicable for FE or RE objects
funnel(trimfill(rma.TF.RE))




#sensitivity analysis/robustness testing
#-- with the leaveout function
# Some table with the robustness. Which study should be excluded for 
# the dataset to remain robust. 

sens.RE = leave1out(rma.RE)
which(sens.RE$I2 == min(sens.RE$I2))
sum(sens.RE$I2 < 25)
hist(sens.RE$I2)
cbind(exp(sens.RE$estimate), sens.RE$pval < 0.05)

sens.RE$I2
