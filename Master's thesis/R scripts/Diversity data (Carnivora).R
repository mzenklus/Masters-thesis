setwd("/Users/matiaszenklusen/Documents/Master's thesis/R scripts")
install.packages("divDyn")
library(divDyn)
install.packages("chronosphere")
library(chronosphere)
dat<-chronosphere::fetch(src="pbdb",ser="occs3")
dat<-dat[dat$class=="Mammalia",]
dat<-dat[dat$order=="Carnivora",]
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

tsplot(stages, boxes="sys", boxes.col="systemCol", 
       shading="series", xlim=c(66, 0), ylim=c(0,4000))
bsMerged <- merge(stages, bs, by="stg")
str(bsMerged)
tsplot(stages, boxes="sys", boxes.col="systemCol", 
       shading="series", xlim=c(66, 0), ylim=c(0,4000), ylab="Number occurrences")
lines(bsMerged$mid, bsMerged$occs)
title ("Carnivora occurrences in the Cenozoic era")

jpeg("Carnivora_genus_richness.jpg", height = 5, width = 9, units = "in", res = 300)
dd <- divDyn(dats, tax="genus", bin="stg")
tsplot(stages, shading="series", boxes="sys",
       xlim=87:95,ylab="Carnivora genus richness", ylim=c(0,200))
lines(stages$mid, dd$divRT, col="black", lwd=2)
title(main="Carnivora genus richness during the Cenozoic era")
abline(a=NULL,b=NULL,h=NULL,v=0.15, col="black", lwd=1.5, lty = 2)
legend("topleft", legend=c("Zoolithenhöhle caves"), 
       col=c("black"), lwd=1.5, lty = 2)
dev.off()

jpeg("Carnivora_per_capita_rates.jpg", height = 5, width = 9, units = "in", res = 300)
tsplot(stages, shading="series", boxes="sys", xlim=87:95,
       ylab="Carnivora per capita turnover rates", ylim=c(0,3))   
lines(stages$mid, dd$extPC, lwd=2, col="red")
lines(stages$mid, dd$oriPC, lwd=2, col="blue")
title(main="Carnivora turnover rates during the Cenozoic era")
abline(a=NULL,b=NULL,h=NULL,v=0.15, col="black", lwd=1.5, lty = 2)
legend("topright", legend=c("extinction", "origination"), 
       col=c("red", "blue"), lwd=2)
legend("topleft", legend=c("Zoolithenhöhle caves"), 
       col=c("black"), lwd=1.5, lty = 2)
dev.off()

dd <- divDyn(dats, tax="genus", bin="stg")
tsplot(stages, shading="series", boxes="sys",
       xlim=87:95,ylab="Carnivora genus richness", ylim=c(0,200))
lines(stages$mid, dd$divRT, col="black", lwd=2)
title(main="Carnivora genus richness during the Cenozoic era")
abline(a=NULL,b=NULL,h=NULL,v=0.15, col="black", lwd=1.5, lty = 2)
legend("topleft", legend=c("Zoolithenhöhle caves"), 
       col=c("black"), lwd=1.5, lty = 2)

tsplot(stages, shading="series", boxes="sys", xlim=87:95,
       ylab="Carnivora per capita turnover rates", ylim=c(0,3))   
lines(stages$mid, dd$extPC, lwd=2, col="red")
lines(stages$mid, dd$oriPC, lwd=2, col="blue")
title(main="Carnivora turnover rates during the Cenozoic era")
abline(a=NULL,b=NULL,h=NULL,v=3.5, col="black", lwd=1.5, lty = 2)
legend("topright", legend=c("extinction", "origination"), 
       col=c("red", "blue"), lwd=2)
legend("topleft", legend=c("Zoolithenhöhle caves"), 
       col=c("black"), lwd=1.5, lty = 2)

write.csv(dats,"~/Documents/PBDB.occurrence.data.Carnivora.csv",row.names=FALSE)
