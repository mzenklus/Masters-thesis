setwd("/Users/matiaszenklusen/Documents/Master's thesis/R scripts")
install.packages("divDyn")
library(divDyn)
install.packages("chronosphere")
library(chronosphere)
dat<-chronosphere::fetch(src="pbdb",ser="occs3")
dat<-dat[dat$class=="Mammalia",]
dat<-dat[dat$order=="Carnivora",]
dat<-dat[dat$family=="Ursidae",]
dat<-dat[!dat$genus=="",]
data(stages)
data(tens)
data(keys)
stgMin <- categorize(dat[ ,"early_interval"], keys$stgInt)
stgMax <- categorize(dat[ ,"late_interval"], keys$stgInt)
stgMin <- as.numeric(stgMin)
stgMax <- as.numeric(stgMax)
dat$stg <- rep(NA, nrow(dat))
stgCondition <- c(
  which(stgMax==stgMin),
  which(stgMax==-1))
dat$stg[stgCondition] <- stgMin[stgCondition]
binMin<-categorize(dat[,"early_interval"],keys$binInt)
binMax<-categorize(dat[,"late_interval"],keys$binInt)
binMin<-as.numeric(binMin)
binMax<-as.numeric(binMax)
dat$bin <- rep(NA, nrow(dat))
binCondition <- c(
  which(binMax==binMin),
  which(binMax==-1))
dat$bin[binCondition] <- binMin[binCondition]
table(dat$stg)
sum(table(dat$stg))
sum(table(dat$stg))/nrow(dat)
dats <- dat[!is.na(dat$stg),]
dats <- dats[dats$stg>82,]
bsFull <- binstat(dats, tax="genus", bin="stg", 
                  coll="collection_no", ref="reference_no",duplicates=FALSE)
bsFull$occs
bs <- binstat(dats, tax="genus", bin="stg", 
              coll="collection_no", ref="reference_no", duplicates=FALSE)

tp <- function(...) tsplot(stages, boxes="sys", boxes.col="systemCol", 
                           shading="series", xlim=82:95, ...)
tp(ylim=c(0,2000), ylab="Number of collections") 
lines(bsMerged$mid, bsMerged$colls)

jpeg("Ursidae_genus_richness.jpg", height = 5, width = 9, units = "in", res = 300)
dd <- divDyn(dats, tax="genus", bin="stg")
tsplot(stages, shading="series", boxes="sys",
       xlim=87:95,ylab="Ursidae genus richness", ylim=c(0,30))
lines(stages$mid, dd$divRT, col="black", lwd=2)
title(main="Ursidae genus richness during the Cenozoic era")
abline(a=NULL,b=NULL,h=NULL,v=0.15, col="black", lwd=1.5, lty = 2)
legend("topleft", legend=c("Zoolithenhöhle caves"), 
       col=c("black"), lwd=1.5, lty = 2)
dev.off()

jpeg("Ursidae_per_capita_rates.jpg", height = 5, width = 9, units = "in", res = 300)
tsplot(stages, shading="series", boxes="sys", xlim=87:95,
       ylab="Ursidae per capita turnover rates", ylim=c(0,3))   
lines(stages$mid, dd$extPC, lwd=2, col="red")
lines(stages$mid, dd$oriPC, lwd=2, col="blue")
title(main="Ursidae turnover rates during the Cenozoic era")
abline(a=NULL,b=NULL,h=NULL,v=0.15, col="black", lwd=1.5, lty = 2)
legend("topright", legend=c("extinction", "origination"), 
       col=c("red", "blue"), lwd=2)
legend("topleft", legend=c("Zoolithenhöhle caves"), 
       col=c("black"), lwd=1.5, lty = 2)
dev.off()

dd <- divDyn(dats, tax="genus", bin="stg")
tsplot(stages, shading="series", boxes="sys",
       xlim=87:95,ylab="Ursidae genus richness", ylim=c(0,30))
lines(stages$mid, dd$divRT, col="black", lwd=2)
title(main="Ursidae genus richness during the Cenozoic era")
abline(a=NULL,b=NULL,h=NULL,v=19, col="black", lwd=1.5, lty = 2)
legend("topleft", legend=c("Zoolithenhöhle caves"), 
       col=c("black"), lwd=1.5, lty = 2)

tsplot(stages, shading="series", boxes="sys", xlim=87:95,
       ylab="Ursidae per capita turnover rates", ylim=c(0,3))   
lines(stages$mid, dd$extPC, lwd=2, col="red")
lines(stages$mid, dd$oriPC, lwd=2, col="blue")
title(main="Ursidae turnover rates during the Cenozoic era")
abline(a=NULL,b=NULL,h=NULL,v=3.2, col="black", lwd=1.5, lty = 2)
legend("topright", legend=c("extinction", "origination"), 
       col=c("red", "blue"), lwd=2)
legend("topleft", legend=c("Zoolithenhöhle caves"), 
       col=c("black"), lwd=1.5, lty = 2)

tsplot(stages, shading="series", boxes="sys", xlim=82:95)
abline(a=NULL,b=NULL,h=NULL,v=43, col="red", lwd=1.5, lty = 2)
abline(a=NULL,b=NULL,h=NULL,v=21, col="red", lwd=1.5, lty=2)

jpeg("Ursidae_per_capita_rates cutoff.jpg", height = 5, width = 9, units = "in", res = 300)
tsplot(stages, shading="series", boxes="sys", xlim=92:95,
       ylab="Ursidae per capita turnover rates", ylim=c(0,3))   
lines(stages$mid, dd$extPC, lwd=2, col="red")
lines(stages$mid, dd$oriPC, lwd=2, col="blue")
title(main="Ursidae turnover rates during the Cenozoic era")
abline(a=NULL,b=NULL,h=NULL,v=1.2, col="black", lwd=1.5, lty = 2)
legend("topright", legend=c("extinction", "origination"), 
       col=c("red", "blue"), lwd=2)
legend("topleft", legend=c("per capita rate cutoff"), 
       col=c("black"), lwd=1.5, lty = 2)
dev.off()

write.csv(dats,"~/Documents/PBDB.occurrence.data.csv",row.names=FALSE)
