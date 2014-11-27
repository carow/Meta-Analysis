library(metafor)
library(meta)

#setwd("C:/Users/ICI/Desktop/MetaAnalysisD")
setwd("/Users/Torfinn/Documents/Uni Freiburg/Best Practice R/Meta-Analysis/R scripts")
table1 <- read.csv2("Gibson.csv")
table2 <- read.csv2("Gibson2.csv")
table1 = Gibson
str(table1)
summary(table1) 

str(table2)

table1=table1[-c(6,7,8,10,12,13)]
table1$d.mean=as.numeric(table1$d.mean)
table1$p.mean=as.numeric(table1$p.mean)
table1$p.sd=as.numeric(table1$p.sd)
table1$d.sd=as.numeric(table1$d.sd)
table1$hedges.g.=as.numeric(table1$hedges.g)



attach(table1)
birds=table1[taxon=="a",-c(6,7,8,10,12,13)]
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

rma.RE = rma(method = "HE", measure = "SMD", m1i = p.mean, m2i = d.mean, sd1i = p.sd, sd2i = d.sd, n1i = p.n, n2i = d.n, vtype = "UB")# maybe change vtype and method
rma.RE

detach(birdsnew)


forest.rma (rma.FE, annotate = TRUE, cex = 0.5, showweight = TRUE) #FE model

forest.rma(rma.RE, annotate = TRUE, cex = 0.5, showweight = TRUE) #RE model


#Causes of heterogeneity - Meta-regression p. 141

attach(birdsnew)

rma.FE.meta = rma(method = "FE", measure = "SMD", m1i = p.mean, m2i = d.mean, sd1i = p.sd, sd2i = d.sd, n1i = p.n, n2i = d.n, vtype = "UB", mods = ~ continent)
rma.FE.meta

#Random Effects Model

# New tests! Adding more moderators to the model: metric of how the biodiversity is measured, type of disturbance.
rma.RE.meta = rma(method = "REML", measure = "SMD", m1i = p.mean, m2i = d.mean, sd1i = p.sd, sd2i = d.sd, n1i = p.n, n2i = d.n, vtype = "UB", mods = ~ continent + metric + disturbance )
save(rma.RE.meta, file="rma.RE.meta")

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

#Higgins 'E model
rma.TF <- rma(method="HE", measure = "SMD", m1i = p.mean, m2i = d.mean, sd1i = p.sd, sd2i = d.sd, n1i = p.n, n2i = d.n) 
trimfill(rma.TF) # Only applicable for FE or RE objects
funnel(trimfill(rma.TF))

#Fixed effects model
rma.TF.FE <- rma(method="FE", measure = "SMD", m1i = p.mean, m2i = d.mean, sd1i = p.sd, sd2i = d.sd, n1i = p.n, n2i = d.n) 
trimfill(rma.TF.FE) # Only applicable for FE or RE objects
funnel(trimfill(rma.TF.FE))

detach(birdsnew)

#sensitivity analysis/robustness testing
#-- with the leaveout function

ordered = sort.list(sens.RE, partial=sens.RE$I2)
top3 = ordered[1:3]

sens.RE$I2


sens.RE = leave1out(rma.RE)
ordersens.RE$I2
which(sens.RE$I2 == min(sens.RE$I2))
sum(sens.RE$I2 < 25)
which(sens.RE$I2 < 25)
hist(sens.RE$I2)
cbind(sens.RE$estimate, sens.RE$pval, sens.RE$pval < 0.05)
sens.RE$I2
rma
which((rma.RE$I2 - sens.RE$I2) > 4)



if ((length(which(sens.RE$I2 < 25))) > 0) {
  (which(sens.RE$I2 < 25))
} else {
      (which((rma.RE$I2 - sens.RE$I2) > 4))}

## If function for selection of important studies bla

if (rma.RE$pval < 0.05) {
  if (length(which(sens.RE$pval > 0.05) > 0)) 
    {(which(sens.RE$pval > 0.05)) 
  } else {
    paste0("No left-out studies yielding non-significance")
  }
} else {
  if (length(which((sens.RE$pval < 0.05) > 0))) { which(sens.RE$pval < 0.05) 
  } else {
    paste("No left-out studies yielding significance")
  }
}


if (rma.RE$pval < 0.05) {(length(which(sens.RE$pval > 0.05)>0)) 
  {(which(sens.RE$pval > 0.05)) 
  } else {
    paste("No left-out studies yielding non-significance")
  }
} else {
  if length(which((sens.RE$pval < 0.05)>0)) { which(sens.RE$pval < 0.05) 
  } else {
    paste("No left-out studies yielding significance")
  }
}

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

# Exploring new plots not previously explored

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

attach(birdsnew)
metacont.REML = metacont(n.e = d.n, mean.e = d.mean, sd.e = d.sd, n.c = p.n, mean.c = p.mean, sd.c = p.sd, method.tau = "REML", label.e = "Disturbed sites", label.c = "Primary forests", sm = "SMD")
metacont.REML.C = metacont(n.e = d.n, mean.e = d.mean, sd.e = d.sd, n.c = p.n, mean.c = p.mean, sd.c = p.sd, method.tau = "REML", label.e = "Disturbed sites", label.c = "Primary forests", sm = "SMD", )


detach(birdsnew)

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

