library(metafor)


setwd("C:/Users/ICI/Documents/Carolina/Uni/Freiburg/3_Semester/Best practice R/Meta Analysis/R scripts")
#setwd("/Users/Torfinn/Documents/Uni Freiburg/Best Practice R/Meta-Analysis/R scripts")

# Load the table through the "import dataset" drop-down menu.

table1 = Gibson
str(table1) # NB! Check if numerical values are recognized as numerical! 

table1=table1[-c(5,6,7,8,10,12,13)]

attach(table1)
birds = table1[taxon=="b",]
detach(table1)
str(table1)

summary(table1) 


# To select randomly one row per study to narrow down the metaanalysis.
library(devtools)
source_gist("https://gist.github.com/mrdwab/6424112")
set.seed(100) 
data.sub = stratified(birds, "study.ID", 1)
summary(data.sub)

#rma of random effects should be called rma.RE, rma of fixed effects should be
#called rma.FE. IF a meta-regression has been conducted, it should be called
#rma.RE.meta or rma.FE.meta respectively. 
#The metafor package needs to be installed

attach(data.sub)

# Our rma analysis
#Fixed Effect Model

rma.FE = rma(method = "FE", measure = "SMD", m1i = p.mean, m2i = d.mean, sd1i = p.sd, sd2i = d.sd, n1i = p.n, n2i = d.n)
rma.FE

#Random Effects Model
rma.RE = rma(method = "REML", measure = "SMD", m1i = p.mean, m2i = d.mean, sd1i = p.sd, sd2i = d.sd, n1i = p.n, n2i = d.n, vtype = "UB")# maybe change vtype and method
rma.RE
save(rma.RE, file="rma.RE")
#http://www.google.de/url?sa=t&rct=j&q=&esrc=s&source=web&cd=5&cad=rja&uact=8&ved=0CEIQFjAE&url=http%3A%2F%2Fwww.researchgate.net%2Fprofile%2FEdward_Purssell%2Fpublication%2F262923251_Meta-analysis_using_metafor_in_R%2Flinks%2F00b7d5395634a8cf6f000000&ei=RWxsVIutCI2tPIHBgZgM&usg=AFQjCNHp1LAEr_ZSwhc9v7Vou-6LdFBlmQ&bvm=bv.80120444,d.ZWU
#
detach(data.sub)



#Causes of heterogeneity - Meta-regression p. 141

attach(data.sub)

rma.FE.meta = rma(method = "FE", measure = "SMD", m1i = p.mean, m2i = d.mean, sd1i = p.sd, sd2i = d.sd, n1i = p.n, n2i = d.n, vtype = "UB", mods = ~ continent)
rma.FE.meta


#Random Effects Model
rma.RE.meta = rma(method = "REML", measure = "SMD", m1i = p.mean, m2i = d.mean, sd1i = p.sd, sd2i = d.sd, n1i = p.n, n2i = d.n, vtype = "UB", mods = ~ continent + metric + disturbance)
rma.RE.meta = rma(method = "REML", measure = "SMD", m1i = p.mean, m2i = d.mean, sd1i = p.sd, sd2i = d.sd, n1i = p.n, n2i = d.n, vtype = "UB", mods = ~ continent + metric + disturbance )
rma.RE.meta

save(rma.RE.meta, file = "rma.RE.meta")

detach(data.sub)


## Automatisation starts here once the two objects have been saved.







####################################
## Extra functions for modelling ###
####################################


# -- Trim fill method
# Only works with certain methods of the RE model. 

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


sens.RE = leave1out(rma.RE)
which(sens.RE$I2 == min(sens.RE$I2))
sum(sens.RE$I2 < 25)
hist(sens.RE$I2)
cbind(exp(sens.RE$estimate), sens.RE$pval < 0.05)

sens.RE$I2

# If function not used
sens.RE$pval

if ((length(which(sens.RE$pval > 0.05))) > 0) {
  (which(sens.RE$pval > 0.05))
} else {
  (which(sens.RE$pval < 0.05))}

hist(sens.RE.str)

if(AIC(m2) < AIC(m1)){
  bestmodel = m2
} else {
  bestmodel = m1
}

#-------------------------------------------------
###############################################
# Exploring new plots not previously explored##
##############################################

#radial plot

radial(rma.RE)


# L'Abbe plot is not applicable to our analysis, as it is for binary outcomes
# labbe(rma.RE) 


qqnorm(rma.RE)

# Baujat plot
# The plot shows the contribution of each study to the overall 
# Q-test statistic for heterogeneity on the x-axis versus the 
# influence of each study (defined as the standardized squared 
# difference between the overall estimate based on a fixed-effects 
# model with and without the study included in the model fitting) on the y-axis.
baujat(rma.RE)

# Doing meta analysis with meta package using the metacont function for continuous values.

z = rstandard(rma.RE)$z
plot(NA, NA, xlim = c(1, k), ylim = c(min(z, -2, 
                                          na.rm = TRUE), max(z, 2, na.rm = TRUE)), xaxt = "n", 
     xlab = "Study", ylab = "", bty = "l")
lines(seq_len(k)[not.na], z[not.na], col = "lightgray")
lines(seq_len(k), z)
points(seq_len(k), z, pch = 21, bg = "black")
axis(side = 1, at = seq_len(k), labels = ids)
abline(h = 0, lty = "dashed")
abline(h = c(qnorm(0.025), qnorm(0.975)), lty = "dotted")

attach(data.sub)
metacont.REML = metacont(n.e = d.n, mean.e = d.mean, sd.e = d.sd, n.c = p.n, mean.c = p.mean, sd.c = p.sd, method.tau = "REML", label.e = "Disturbed sites", label.c = "Primary forests", sm = "SMD")
metacont.REML.C = metacont(n.e = d.n, mean.e = d.mean, sd.e = d.sd, n.c = p.n, mean.c = p.mean, sd.c = p.sd, method.tau = "REML", label.e = "Disturbed sites", label.c = "Primary forests", sm = "SMD", )


detach(data.sub)

funnel(metacont.REML)

print.meta(metacont.REML)
#metareg(metacont.REML)
#bubble(metacont.REML)

dev.copy2pdf(width = 15, height = 15,out.type = "pdf")
forest(metacont.REML)

tf1 = trimfill(metacont.REML)
funnel(tf1)
tf1

save(birdsnew, file = "/Users/Torfinn/Documents/Uni Freiburg/Best Practice R/Meta-Analysis/R scripts/birdsnew")
save(birds, file = "/Users/Torfinn/Documents/Uni Freiburg/Best Practice R/Meta-Analysis/R scripts/birds")




# Bubble plot with use of the meta package. If more than two groups it produces one plot 
# per extra group, so not necessarily very useful.

attach(birds)
metacont.REML = metacont(n.e = d.n, mean.e = d.mean, sd.e = d.sd, n.c = p.n, mean.c = p.mean, sd.c = p.sd, method.tau = "REML", label.e = "Disturbed sites", label.c = "Primary forests", sm = "SMD", data = birdsnew)
metacont.upd = update(metacont.REML, byvar = continent)
par(mfrow=c(1,3))
bubble(metareg(metacont.upd))
par(mfrow=c(1,1))

detach(birds)




\begin{figure}
\centering
<<label=Diagnostics2, echo=FALSE, fig=TRUE>>=
  
  op2 <- par(mfrow = c(2, 1), 
             pty = "s", mar=c(5,5,4,1))       # square plotting region,
# independent of device size

## At end of plotting, reset to previous settings:
par(op2)
#Standardized residuals plot does not have a ready-made function, and must be coded. Code used is from the plot.rma.uni.
qqnorm(rma.RE, label = "all", pch = NA_integer_)

@
  \caption{Two diagnostic plots for meta analysis asdlfkmasd;lfk.}
\end{figure}

par (mfrow = c(2, 1), mar=c(5,5,1,1)) #mar=c(5,5,4,1)

?lscape
