setwd("/Users/matiaszenklusen/Documents/Master's thesis")
bear_modified<-read.csv("Data/General cranial morphology data-modified.csv")
Guloloch_modified<-subset(bear_modified,bear_modified$Cave=="Guloloch")
Bärenkammer_modified<-subset(bear_modified,bear_modified$Cave=="Bärenkammer")
Schacht_und_Spalte_modified<-subset(bear_modified,bear_modified$Cave=="Schacht & Spalte")
Neue_Spalte_modified<-subset(bear_modified,bear_modified$Cave=="Neue Spalte")

#Scatterplot

Guloloch.cranial.length.cm<-Guloloch_modified$Cranial_length_cm
Guloloch.maximum.maxillary.width<-Guloloch_modified$Maximum_maxillary_width_cm
plot(Guloloch.cranial.length.cm, Guloloch.maximum.maxillary.width, main="Maximum maxillary width (cm) vs. 
     Cranial length (cm) (Guloloch)",
     xlab="Cranial length (cm)", ylab="Maximum maxillary width (cm)", pch=19, frame=FALSE)
abline(lm(Guloloch.maximum.maxillary.width~Guloloch.cranial.length.cm, data=Guloloch_modified),col="blue")

Bärenkammer.cranial.length.cm<-Bärenkammer_modified$Cranial_length_cm
Bärenkammer.maximum.maxillary.width<-Bärenkammer_modified$Maximum_maxillary_width_cm
plot(Bärenkammer.cranial.length.cm, Bärenkammer.maximum.maxillary.width, main="Maximum maxillary width (cm) vs. 
     Cranial length (cm) (Bärenkammer)",
     xlab="Cranial length (cm)", ylab="Maximum maxillary width (cm)", pch=19, frame=FALSE)
abline(lm(Bärenkammer.maximum.maxillary.width~Bärenkammer.cranial.length.cm, data=Bärenkammer_modified),col="blue")

Schacht.und.Spalte.cranial.length.cm<-Schacht_und_Spalte_modified$Cranial_length_cm
Schacht.und.Spalte.maximum.maxillary.width<-Schacht_und_Spalte_modified$Maximum_maxillary_width_cm
plot(Schacht.und.Spalte.cranial.length.cm, Schacht.und.Spalte.maximum.maxillary.width, main="Maximum maxillary width (cm) vs. 
     Cranial length (cm) (Schacht und Spalte)",
     xlab="Cranial length (cm)", ylab="Maximum maxillary width (cm)", pch=19, frame=FALSE)
abline(lm(Schacht.und.Spalte.maximum.maxillary.width~Schacht.und.Spalte.cranial.length.cm, data=Schacht_und_Spalte_modified),col="blue")

Neue.Spalte.cranial.length.cm<-Neue_Spalte_modified$Cranial_length_cm
Neue.Spalte.maximum.maxillary.width<-Neue_Spalte_modified$Maximum_maxillary_width_cm
plot(Neue.Spalte.cranial.length.cm, Neue.Spalte.maximum.maxillary.width, main="Maximum maxillary width (cm) vs. 
     Cranial length (cm) (Neue Spalte)",
     xlab="Cranial length (cm)", ylab="Maximum maxillary width (cm)", pch=19, frame=FALSE)
abline(lm(Neue.Spalte.maximum.maxillary.width~Neue.Spalte.cranial.length.cm, data=Neue_Spalte_modified),col="blue")
