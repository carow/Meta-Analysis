mT <- metacont.REML
save(mT, file="metacontTemplate.Rdata")

rma2meta <- function(rma.object){
  load("metacontTemplate")
  mT$n.e = NA
  ...
  mT$effectsize = rma.object$vi
  
}

meta2rma