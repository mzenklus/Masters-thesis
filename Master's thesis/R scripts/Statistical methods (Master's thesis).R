setwd("/Users/matiaszenklusen/Documents/Master's thesis")
bear_modified<-read.csv("Data/General cranial morphology data-modified.csv")
Guloloch_modified<-subset(bear_modified,bear_modified$Cave=="Guloloch")
Bärenkammer_modified<-subset(bear_modified,bear_modified$Cave=="Bärenkammer")
Schacht_und_Spalte_modified<-subset(bear_modified,bear_modified$Cave=="Schacht & Spalte")
Neue_Spalte_modified<-subset(bear_modified,bear_modified$Cave=="Neue Spalte")

#Boxplot

library(ggplot2)

jpeg("Cranial length (cm) boxplot.jpeg")

ggplot(bear_modified,aes(x=Cave,y=Cranial_length_cm))+
  geom_boxplot(fill="slateblue")+
  xlab("Cave")+
  ylim(25,45)

dev.off()

jpeg("Cranial height (cm) boxplot.jpeg")

ggplot(bear_modified,aes(x=Cave,y=Cranial_height_cm))+
  geom_boxplot(fill="slateblue")+
  xlab("Cave")+
  ylim(5,15)

dev.off()

jpeg("Maximum maxillary width (cm) boxplot.jpeg")

ggplot(bear_modified,aes(x=Cave,y=Maximum_maxillary_width_cm))+
  geom_boxplot(fill="slateblue")+
  xlab("Cave")+
  ylim(7,12)

dev.off()

jpeg("Orbital width (cm) boxplot.jpeg")

ggplot(bear_modified,aes(x=Cave,y=Orbital_width_cm))+
  geom_boxplot(fill="slateblue")+
  xlab("Cave")+
  ylim(10,20)

dev.off()

jpeg("Orbit average (cm) boxplot.jpeg")

ggplot(bear_modified,aes(x=Cave,y=Orbit_average_cm))+
  geom_boxplot(fill="slateblue")+
  xlab("Cave")+
  ylim(5,8)

dev.off()

#anova
anova.cranial.length.modified<-aov(Cranial_length_cm~Cave,data=bear_modified)
summary(anova.cranial.length.modified)

anova.cranial.height.modified<-aov(Cranial_height_cm~Cave,data=bear_modified)
summary(anova.cranial.height.modified)

anova.maximum.maxillary.width.modified<-aov(Maximum_maxillary_width_cm~Cave,data=bear_modified)
summary(anova.maximum.maxillary.width.modified)

anova.orbit.average.modified<-aov(Orbit_average_cm~Cave,data=bear_modified)
summary(anova.orbit.average.modified)

anova.orbital.width.modified<-aov(Orbital_width_cm~Cave,data=bear_modified)
summary(anova.orbital.width.modified)

#Tukey tests 
TukeyHSD(anova.cranial.length.modified)

TukeyHSD(anova.cranial.height.modified)

TukeyHSD(anova.maximum.maxillary.width.modified)

TukeyHSD(anova.orbital.width.modified)

TukeyHSD(anova.orbit.average.modified)

#Cranial measurements linear regression

#Maximum maxillary width vs. cranial length across caves 

lmmaxGuloloch.modified<-lm(Maximum_maxillary_width_cm~Cranial_length_cm,data=Guloloch_modified)
summary(lmmaxGuloloch.modified)

lmmaxBärenkammer.modified<-lm(Maximum_maxillary_width_cm~Cranial_length_cm,data=Bärenkammer_modified)
summary(lmmaxBärenkammer.modified)

lmmaxSchachtSpalte.modified<-lm(Maximum_maxillary_width_cm~Cranial_length_cm,data=Schacht_und_Spalte_modified)
summary(lmmaxSchachtSpalte.modified)

lmmaxNeueSpalte.modified<-lm(Maximum_maxillary_width_cm~Cranial_length_cm,data=Neue_Spalte_modified)
summary(lmmaxNeueSpalte.modified)

#Orbital width vs. cranial length across caves 

lmorbGuloloch.modified<-lm(Orbital_width_cm~Cranial_length_cm,data=Guloloch_modified)
summary(lmorbGuloloch.modified)

lmorbBärenkammer.modified<-lm(Orbital_width_cm~Cranial_length_cm,data=Bärenkammer_modified)
summary(lmorbBärenkammer.modified)

lmorbSchachtSpalte.modified<-lm(Orbital_width_cm~Cranial_length_cm,data=Schacht_und_Spalte_modified)
summary(lmorbSchachtSpalte.modified)

lmorbNeueSpalte.modified<-lm(Orbital_width_cm~Cranial_length_cm,data=Neue_Spalte_modified)
summary(lmorbNeueSpalte.modified)

#Cranial height vs. cranial length across caves 

lmheightGuloloch.modified<-lm(Cranial_height_cm~Cranial_length_cm,data=Guloloch_modified)
summary(lmheightGuloloch.modified)

lmheightBärenkammer.modified<-lm(Cranial_height_cm~Cranial_length_cm,data=Bärenkammer_modified)
summary(lmheightBärenkammer.modified)

lmheightSchachtSpalte.modified<-lm(Cranial_height_cm~Cranial_length_cm,data=Schacht_und_Spalte_modified)
summary(lmheightSchachtSpalte.modified)

lmheightNeueSpalte.modified<-lm(Cranial_height_cm~Cranial_length_cm,data=Neue_Spalte_modified)
summary(lmheightNeueSpalte.modified)

#Orbit average vs. cranial length across caves 

lmorbavGuloloch.modified<-lm(Orbit_average_cm~Cranial_length_cm,data=Guloloch_modified)
summary(lmorbavGuloloch.modified)

lmorbavBärenkammer.modified<-lm(Orbit_average_cm~Cranial_length_cm,data=Bärenkammer_modified)
summary(lmorbavBärenkammer.modified)

lmorbavSchachtSpalte.modified<-lm(Orbit_average_cm~Cranial_length_cm,data=Schacht_und_Spalte_modified)
summary(lmorbavSchachtSpalte.modified)

lmorbavNeueSpalte.modified<-lm(Orbit_average_cm~Cranial_length_cm,data=Neue_Spalte_modified)
summary(lmorbavNeueSpalte.modified)

#Linear regression overall dataset

lmmax.modified<-lm(Maximum_maxillary_width_cm~Cranial_length_cm,data=bear_modified)
summary(lmmax.modified)

lmorbav.modified<-lm(Orbit_average_cm~Cranial_length_cm,data=bear_modified)
summary(lmorbav.modified)

lmorb.modified<-lm(Orbital_width_cm~Cranial_length_cm,data=bear_modified)
summary(lmorb.modified)

lmheight.modified<-lm(Cranial_height_cm~Cranial_length_cm, data=bear_modified)
summary(lmheight.modified)

#PCA

library(corrr)
library(ggcorrplot)
library(FactoMineR)
library(factoextra)

str(bear_modified)

bear_modified<-na.omit(bear_modified)

pca.dataframe2<-bear_modified
head(pca.dataframe2)

pca.dataframe2.modified<-pca.dataframe2[,4:8]
head(pca.dataframe2.modified)

pca.dataframe2.scaled<-scale(pca.dataframe2.modified)
head(pca.dataframe2.scaled)

bear_modified_pca<-prcomp(pca.dataframe2.scaled,center=TRUE,scale.=TRUE)
summary(bear_modified_pca)

#Scree plot

jpeg(file="Scree plot (bear_modified_pca).jpeg")

fviz_eig(bear_modified_pca,addlabels=TRUE)

dev.off()

#PCA biplot

jpeg(file="Biplot(bear_modified) with convex hulls.jpeg")

fviz_pca_biplot(bear_modified_pca,
                col.ind=pca.dataframe2$Cave,
                col.var="black",label="var",
                legend.title="Cave",
                addEllipses = TRUE,
                ellipse.type="convex")+
  labs(title="Biplot of PCA results")

dev.off()

#Cos2 plot

jpeg(file="Cos2 plot (bear_modified_pca).jpeg")

fviz_cos2(bear_modified_pca,choice="var",axes=1:2)

dev.off()

#Eigenvalue scree plot

eigenvalues.bear.modified.pca<-bear_modified_pca$sdev

jpeg(file="Eigenvalue scree plot (bear_modified_pca).jpeg")

plot(eigenvalues.bear.modified.pca,type="b", xlab="Principal component",ylab="Eigenvalue")
abline(v=2,col="red")

dev.off()
