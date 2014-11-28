# This script shows how we blabla


# Load the two objects, rma.RE & rma.RE.meta
load("C:/Users/ICI/Documents/Carolina/Uni/Freiburg/3_Semester/Best practice R/Meta-Analysis/R scripts/SweaveTorfinn/rma.RE")
load("C:/Users/ICI/Documents/Carolina/Uni/Freiburg/3_Semester/Best practice R/Meta-Analysis/R scripts/SweaveTorfinn/rma.RE.meta")

library(metafor)
library(xtable)


#Number of Studies in the RE model of the meta-analysis
n.studies = rma.RE$k


# Number of studies that should be reported in the sensitivity analysis:
n.sens = as.integer(rma.RE$k*0.1+1)


#Egger test and fail-safe number
egger.RE = regtest(rma.RE, model = "rma", predictor = "sei")
fsn.RE = fsn(yi = rma.RE$yi, vi = rma.RE$vi, type = "Rosenberg")


# Table of meta-analysis
matrix.RE = data.frame(t(c("Effect Size" = rma.RE$b, "SE of Effect Size" = rma.RE$se, "CI1" = rma.RE$ci.lb, "CI2" = rma.RE$ci.ub, "P(ES)" = rma.RE$pval, "Q" = rma.RE$QE, "P(Q)" = rma.RE$QEp, "I^2" = rma.RE$I2, "Egger" = egger.RE$zval, "P(Egger)" = egger.RE$pval, "FSN" = fsn.RE$fsnum)))

rownames(matrix.RE) = c("Meta-Analysis")

colnames(matrix.RE) = c("{ES}","\\parbox{1cm}{SE of ES}", "CI (lb)", "CI (ub)", "P(ES)", "Q", "P(Q)", "$I^2$", "Egger", "P(Egger)","FSN")

require(xtable)
mytable = matrix.RE

print(xtable(mytable, caption = paste("Results of the meta-analysis. ES = Effect Size, ES = Standard error, CI = 95 \\% confidenc interval, P(ES) = p-value of ES, Q = Test for residual heterogeneity, $I^2$ = Residual heterogeneity, Egger's test (SE used as the predictor) and the fails-safe number (FSN) for publication bias testing according to 'Rosenberg'."), align = "rccccccccccc", label = "xtable1"),caption.placement = "top", size ="footnotesize", scalebox = 0.9, sanitize.text = force, display = "g")


#If function egger and fsn

if (egger.RE$pval < 0.05){
  out = paste0("The Eggers test for testing funnel plot asymmetry suggests that there is a probable publication bias present because there is a significant relationship between the effect size and the sample size. Asymmetry exists in the funnel plot")
} else {
  out = paste0("The Eggers test for testing funnel plot asymmetry suggests that there is no publication bias because there is no significant relationship between the effect size and the sample size. The funnel plot is symmetrical")
}


if (fsn.RE$fsnum > ((5*(n.studies)) + 10)){
  out = paste0("The fail-safe number suggests that the meta-analysis is robust against publication bias")
} else {
  out = paste0("The fail-safe number suggests that the meta-analysis is not robust against publication bias")}


#Forest plot of the meta-analysis

par(mar = c(3,4,1,2))

ci.ub = max(rma.RE$yi + qnorm(0.05/2, lower.tail = FALSE) * sqrt(rma.RE$vi))
ci.lb = min(rma.RE$yi - qnorm(0.05/2, lower.tail = FALSE) * sqrt(rma.RE$vi))

forest.rma(rma.RE, annotate = TRUE, cex = 0.7, showweight = TRUE, addcred = TRUE, xlab = "", xlim = c((4*ci.lb), (3*ci.ub)))
text(0, rma.RE$k+2, "Standardized Mean Difference", cex = .68, font = 2)
text(4*ci.lb, rma.RE$k+2, "Study",    pos = 4, cex = .68, font = 2)
text(3*ci.ub, rma.RE$k+2, "Weights   Effect sizes [95% CI]",  pos = 2, cex = .68, font = 2)
par (mar = c(5,4,4,2))


# Meta-regression

matrix.RE.meta = data.frame("ES" = as.vector(rma.RE.meta$b),
                            "SEES"=as.vector(rma.RE.meta$se),
                            "CI1"= as.vector(rma.RE.meta$ci.lb), 
                            "CI2" = as.vector(rma.RE.meta$ci.ub), 
                            "pES" = as.vector(rma.RE.meta$pval),
                            "Q" = rma.RE.meta$QE,
                            "P(Q)" = rma.RE.meta$QEp,
                            "I^2" = rma.RE.meta$I2,
                            "Mods" = rma.RE.meta$QM,
                            "P(Mods)" = rma.RE.meta$QMp )
matrix.RE.meta[2:NROW(matrix.RE.meta), 6:10] = NA


rownames(matrix.RE.meta) = attr(rma.RE.meta$b, "dimnames")[[1]] 

colnames(matrix.RE.meta) = c("{ES}","\\parbox{1cm}{SE of ES}","CI (lb)", "CI (ub)", "P(ES)", "Q", "P(Q)", "I^2", "QM", "P(QM)")

require(xtable)
mytable2 = matrix.RE.meta

  

print(xtable(mytable2, 
             caption = paste("Results of the meta-regression (mixed-effects model). The model results are shown taking a moderator or various moderators into account and displaying their coefficients. Results for the whole model are displayed as Q = Test for residual heterogeneity, $I^2$ = residual heterogeneity and QM = Test of Moderators."), 
               align = "rccccc|ccccc", digits = 3, display= c("s", rep("g",10)), label = "xtable2"), caption.placement = "top", size ="footnotesize", scalebox = 0.9, sanitize.text = force)


#Forest plot of the meta-regression


par(mar = c(3,4,1,2))
ci.ub = max(rma.RE$yi + qnorm(0.05/2, lower.tail = FALSE) * sqrt(rma.RE$vi))
ci.lb = min(rma.RE$yi - qnorm(0.05/2, lower.tail = FALSE) * sqrt(rma.RE$vi))

forest.rma(rma.RE.meta, annotate = TRUE, cex = 0.7, showweight = TRUE, addcred = TRUE, xlab = "", xlim = c((4*ci.lb), (3*ci.ub))) #  c(-ci.lb, ci.ub)
text(0, rma.RE$k+2, "Standardized Mean Difference", cex = .68, font = 2)
text(4*ci.lb, rma.RE$k+2, "Study",    pos = 4, cex = .68, font = 2)
text(3*ci.ub, rma.RE$k+2, "Weights   Effect sizes [95% CI]",  pos = 2, cex = .68, font = 2)
par (mar = c(5,4,4,2))


#Funnel plot

funnel(rma.RE)

#Diagnostics

#Standardized residuals plot does not have a ready-made function, and must be coded. Code used is from the plot.rma.uni.
par (mfrow= c(2,2), mar = c(4,4,1,2))
par(mar = c(4,4,2,1))
z = rstandard(rma.RE)$z
k <- length(z)
plot(NA, NA, xlim = c(1, k), ylim = c(min(z, -2, na.rm = TRUE), max(z, 2, na.rm = TRUE)), xaxt = "n", xlab = "Study", ylab = "Standardized residuals", bty = "l", main="Standardized Residuals Plot", mgp = c(2,1,0))
lines(seq_len(k), z, col = "lightgray")
lines(seq_len(k), z)
points(seq_len(k), z, pch = 21, bg = "black")
axis(side = 1, at = seq_len(k), labels = rma.RE$ids)
abline(h = 0, lty = "dashed")
abline(h = c(qnorm(0.025), qnorm(0.975)), lty = "dotted")

#q-q normal plot
qqnorm(rma.RE, label = "all", mgp = c(2,1,0))

#Baujat plot
par(mar = c(4,3,2,1))
baujat(rma.RE, main = "Baujat Heterogenity Plot", mgp = c(2,1,0))

#Radial plot
par(mar = c(0,0,0,0))
radial(rma.RE, zlab = "", xlab = "Inverse standard error", cex = 1, mgp = c(2,1,0), main = "Galbraith's Radial Plot")
#title("Galbraith's adial plot", line = -6)
mtext("ES/SE", side = 2, line = -1, at = 0, cex=0.8)

par(mfrow = c(1,1), mar = c(4,4,1,2))


#Sensitivity Analysis

weight.RE = weights(rma.RE) #shows the weighting of each study


sens.RE = leave1out(rma.RE)

matrix.RE.sens = data.frame("Study" = as.vector (sens.RE$slab) ,
                            "ES" = as.vector (sens.RE$estimate),                            
                            "ESchg" = as.vector (sens.RE$estimate - rma.RE$b),
                            "SEES"=as.vector (sens.RE$se),
                            "CI1"= as.vector (sens.RE$ci.lb), 
                            "CI2" = as.vector (sens.RE$ci.ub), 
                            "pES" = as.vector (sens.RE$pval),
                            "Q" = as.vector (sens.RE$Q),
                            "pQ" = as.vector (sens.RE$Qp),
                            "I^2" = as.vector (sens.RE$I2),
                            "ESchgabs"=as.vector (abs(sens.RE$estimate - rma.RE$b)),                          
                            "Weight" = as.vector(weight.RE) )

matrix.RE.sens = matrix.RE.sens[order(matrix.RE.sens$ESchgabs, decreasing = TRUE),]
matrix.RE.sens = matrix.RE.sens[1:n.sens,]
matrix.RE.sens = matrix.RE.sens[-c (11)]


rownames(matrix.RE.sens) = matrix.RE.sens$slab  

colnames(matrix.RE.sens) = c("\\parbox{1.05cm}{Left-out Study}", "{ES}", "ES change", "\\parbox{1cm}{SE of ES}","CI (lb)", "CI (ub)", "P(ES)", "Q", "P(Q)", "I^2", "\\parbox{1.05 cm}{Weight of Study}")

require(xtable)
mytable3 = matrix.RE.sens

  
print(xtable(mytable3, 
               caption = paste("Output of the sensitivity analysis (leave1out analysis) with the results of the meta analysis, leaving out 1 study at a time. The top most influential studies, in relation to effect size, sorted by absolute change in effect size is shown. The Left-out study indicates which study is left out to produce the results. ES = Effect size, SE = Standard error, CI =  95 \\% confidence interval, P(ES) = p-value of estimate, Q = Test for residual heterogeneity, P(Q) = p-value of heterogenity value, $I^2$ = residual heterogeneity and weight given in the meta-analysis (\\%)"),  align = "rccccccccccc", digits = 3, display= c(rep("g",12)),label = "xtable3"), 
        caption.placement = "top", size ="footnotesize", scalebox = 1, sanitize.text = force, include.rownames = getOption("xtable.include.rownames", F))

# If function p-value meta-analysis
if(rma.RE$pval < 0.05) {
  out1 = paste("The effect size found in this meta-analysis is significant with a p-value of")
} else {
  out1 = paste ("The effect size found in this meta-analysis is non-significant with a p-value of")
}


#If funtcion for sensitivity analysis (leave1out)

if (rma.RE$pval < 0.05) {
  if (length(which(sens.RE$pval > 0.05) > 0)) { 
    out2 <- paste("The leave-one out analysis shows that the effect size of the meta-analysis is not significant anymore if", 
                  if (sum(sens.RE$pval > 0.05)>1) {"studies"} 
                  else {"study"}, paste(which(sens.RE$pval > 0.05), collapse=", "), 
                  (if (sum(sens.RE$pval > 0.05)>1) {"are"} 
                   else {"is"}), "left out. If there are many left-out studies yielding non-significance relative to the total number of studies in the meta analysis, it is a sign of a fragile analysis")   
  } else {
    out2 <- paste("The leave-one out analysis shows that no left-out studies yield non-significance. This means that the finding of significance in effect size of meta-analysis is robust")
  }
} else {  
  if (length(which(sens.RE$pval < 0.05) > 0)) {   
    out2 = paste("The leave-one out analysis shows that significance 
                 is yielded by leaving out", 
                 if (sum(sens.RE$pval > 0.05)>1) {"studies"} 
                 else {"study"}, paste(which(sens.RE$pval < 0.05), collapse=","),". 
                 If there are many left-out studies yielding significance, relative to the total number of studies in the meta-analysis, it is a sign that the p-value of the meta-analysis is nearly significant. One should, however, be cautious if one leaves out a study to make the  analysis significant. A significant effect size does not necessarily mean a robust analysis.")  
  } 
  else {
    out2 = paste("The leave-one out analysis shows that no left-out studies yield significance of the original meta-analysis. This is a sign that the finding of non-significance in effect size is robust")}
  }