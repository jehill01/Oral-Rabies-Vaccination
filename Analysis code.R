library(dplyr)
library(tidyr)
library(lme4)
library(MuMIn)
library(emmeans)
library(ggplot2); library(ggeffects)
library(ggpubr)
library(marginaleffects)
source("outputfunction.R")

#Read in and format datafile
data<-read.csv("baitingdata3.csv") #individual data
datagrid<-read.csv("datagrid3.csv") #grid level data [number pos and neg for biomarker]
data$Area<-as.factor(data$Area)
data$Raccoon<-scale(data$Raccoon, center=TRUE)
data$Opossum<-scale(data$Opossum, center=TRUE)
data$Density<-as.factor(data$Density)
datagrid$Area<-as.factor(datagrid$Area)
datagrid$Raccoon<-scale(datagrid$Raccoon, center=TRUE)
datagrid$Opossum<-scale(datagrid$Opossum, center=TRUE)
datagrid$Density<-as.factor(datagrid$Density)

#Analysis #1
#Spring: increasing area from 0.5 to 3 km2

#Analysis 1:both species
spring <- subset(data, Season=="Spring"& Density==75)

bothspring<-(glmer(Biomarker~Species+Area+Habitat+Raccoon+Habitat:Species+
                     Opossum+Species:Area+(1|Year), 
                family=binomial(link="logit"), na.action=na.pass, data=spring))

output(bothspring)
tablemodel
bothfitspring<-(glmer(Biomarker~Habitat+Species+(1|Year), 
                   family=binomial(link="logit"), na.action=na.pass, data=spring))

ggpredict(bothfitspring, terms=c("Species"), condition = c(Habitat="Bottomland") )
ggpredict(bothfitspring, terms=c("Species"), condition = c(Habitat="Pine") )
ggpredict(bothfitspring, terms=c("Species"), condition = c(Habitat="Riparian") )
ggpredict(bothfitspring, terms=c("Species"), condition = c(Habitat="Wetland") )

emmeans(bothfitspring, pairwise~Habitat, type="response")
r.squaredGLMM(bothfitspring)

##Analysis 1: raccoons only
springraccoon<- subset(spring, Species=="Raccoon")
raccspringfull<-(glmer(Biomarker~Sex+Area*Habitat+Opossum+
                         Raccoon+Residency+(1|Year), 
                       family=binomial(link="logit"), na.action=na.pass, data=springraccoon))

output(raccspringfull)

raccspringfit<-(glmer(Biomarker~Sex+Area*Habitat+
                         Raccoon+(1|Year), 
                       family=binomial(link="logit"), na.action=na.pass, data=springraccoon))


ggpredict(raccspringfit, terms=c("Area"), condition = c(Habitat="Bottomland", Raccoon=5.8, Sex="M"))
ggpredict(raccspringfit, terms=c("Area"), condition = c(Habitat="Bottomland", Raccoon=5.8, Sex="F"))
ggpredict(raccspringfit, terms=c("Area"), condition = c(Habitat="Riparian", Raccoon=5.8, Sex="M"))
ggpredict(raccspringfit, terms=c("Area"), condition = c(Habitat="Riparian", Raccoon=5.8, Sex="F"))
ggpredict(raccspringfit, terms=c("Area"), condition = c(Habitat="Pine", Raccoon=5.8, Sex="M"))
ggpredict(raccspringfit, terms=c("Area"), condition = c(Habitat="Pine", Raccoon=5.8, Sex="F"))
ggpredict(raccspringfit, terms=c("Area"), condition = c(Habitat="Wetland", Raccoon=5.8, Sex="M"))
ggpredict(raccspringfit, terms=c("Area"), condition = c(Habitat="Wetland", Raccoon=5.8, Sex="F"))

ggpredict(raccspringfit, terms=c("Area"), condition = c(Habitat="Wetland", Raccoon=2.1, Sex="M"))
ggpredict(raccspringfit, terms=c("Sex"), condition = c(Habitat="Bottomland", Raccoon=3.16, Area=.5))
ggpredict(raccspringfit, terms=c("Area"), condition = c(Habitat="Riparian", Raccoon=5, Sex="M"))
ggpredict(raccspringfit, terms=c("Raccoon"), condition = c(Habitat="Riparian", Area=3, Sex="M"))




##Analysis 1: proportion positive
springgrid<-subset(datagrid, Season=="Spring"& Density==75)

prop.reg<-glmer(formula=cbind(Pos, Sum-Pos)~Area+Habitat+Area:Habitat+scale(Raccoon)+
                  Opossum+(1|Year)+(1|Grid), 
                weights=Sum, data=springgrid, na.action=na.pass, family=binomial())

output(prop.reg)
prop.regfit<-glmer(formula=cbind(Pos, Sum-Pos)~as.factor(Area)*Habitat+Raccoon+(1|Year)+(1|Grid), 
                weights=Sum, data=springgrid, na.action=na.pass, family=binomial())

r.squaredGLMM(prop.regfit)

#Prediction of effects
ggpredict(prop.regfit, terms=c("Area"), condition = c(Habitat="Bottomland", Raccoon=2.1))
ggpredict(prop.regfit, terms=c("Area"), condition = c(Habitat="Riparian", Raccoon=2.1))
ggpredict(prop.regfit, terms=c("Raccoon[all]"), condition = c(Habitat="Bottomland", Area=0.5))
ggpredict(prop.regfit, terms=c("Raccoon[all]"), condition = c(Habitat="Bottomland", Area=3))

#Predictions overall
ggemmeans(prop.regfit, terms=c("Habitat"), condition=c(Area=0.5, Raccoon=5.8))
ggemmeans(prop.regfit, terms=c("Habitat"), condition=c(Area=3, Raccoon=5.8))

emmeans(prop.regfit, pairwise~Area|Habitat, type="response")
emmip(prop.regfit, ~Area|Habitat, type="response")


springgrid$PropPos<-(springgrid$Pos/(springgrid$Pos+springgrid$Neg))
Pos<-springgrid%>% # talbe of mean pos for biomarker
  group_by(Habitat, Area) %>%
  summarise(meanpos=mean(PropPos),
            sd=sd(PropPos)
  )

#Analysis 2: Effects of Bait Density and Season

#Analysis 2: Both species
new <- data[!(data$Year<2020),]

bothdensity<-(glm(Biomarker~Species*Season+Species*Habitat+Species*Density+Density*Habitat+
                    Density*Season+Season*Habitat+Opossum+Raccoon, 
                    family=binomial(link="logit"), na.action=na.pass, data=new))
bothdensityfit<-(glm(Biomarker~Species+Density+Season+scale(Opossum), 
                    family=binomial(link="logit"), na.action=na.pass, data=new))

output(bothdensity)
r.squaredGLMM(bothdensityfit)
summary(bothdensityfit)

#predictions of effects
ggpredict(bothdensityfit, terms=c("Species"), condition = c(Season="Spring", Opossum=3.1))
ggpredict(bothdensityfit, terms=c("Season"), condition = c(Species="Raccoon", Opossum=3.1))
ggpredict(bothdensityfit, terms=c("Density"), condition = c(Species="Raccoon", Opossum=3.1))
ggpredict(bothdensityfit, terms=c("Opossum"), condition = c(Species="Raccoon"))

#predictions across habitats
ggpredict(bothdensityfit, terms=c("Species"), condition = c(Season="Spring", Opossum=4.3, Density=75))
ggpredict(bothdensityfit, terms=c("Species"), condition = c(Season="Spring", Opossum=4.3, Density=150))
ggpredict(bothdensityfit, terms=c("Species"), condition = c(Season="Fall", Opossum=5.6, Density=75))
ggpredict(bothdensityfit, terms=c("Species"), condition = c(Season="Fall", Opossum=5.6, Density=150))

#Analysis 2: raccoons only
newracc<-subset(new, Species=="Raccoon" & Sex!="")

raccdensity<-(glm(Biomarker~Sex+Opossum+Raccoon+Residency+
                      Season*Density*Habitat, 
                  family=binomial(link="logit"), na.action=na.pass, data=newracc))

output(raccdensity)
dredge(raccdensity)
raccfit <-(glm(Biomarker~Season*Density+scale(Opossum, center=TRUE)+Residency, 
               family=binomial(link="logit"), na.action=na.pass, data=newracc))
r.squaredGLMM(raccfit)
summary(raccfit)

#Prediction of effects
ggpredict(raccfit, terms=c("Season"), condition = c(Opossum=3.48, Density="75", Residency=2))
ggpredict(raccfit, terms=c("Density"), condition = c(Season="Spring", Opossum=3.48, Residency=2))
ggpredict(raccfit, terms=c("Residency"), condition = c(Opossum=1, Season="Fall"))
ggpredict(raccfit, terms=c("Opossum"), condition = c(Season="Fall"))

#Predictions overall
ggpredict(raccfit, terms=c("Season"), condition = c(Opossum=5, Density=75, Residency=1.717))
ggpredict(raccfit, terms=c("Season"), condition = c(Opossum=5, Density=150, Residency=1.717))



emmip(raccfit, ~Density|Season, type="response")
emmeans(raccfit, pairwise~Season|Density, type="response")


#Analysis 2: Grid-level analysis
newgrid <- datagrid[!(datagrid$Year<2020),]
mixed<-glmer(formula=cbind(Pos, Sum-Pos)~Opossum+as.factor(Density)*Season*Habitat+
                 scale(Raccoon)+(1|Grid), 
               weights=Sum, data=newgrid, na.action=na.pass, family=binomial())
output(mixed)
r.squaredGLMM(mixed)

emmeans(mixed, pairwise~Density|Season|Habitat, type="response")
emmip(mixed, ~Density|Season|Habitat, CIs=TRUE, CIarg=aes(lwd=1, color="black", linetype=1), type="response")
emmeans(mixed, pairwise~Season|Density|Habitat, type="response")
emmip(mixed, ~Season|Density|Habitat, CIs=TRUE, CIarg=aes(lwd=1, color="black", linetype=1), type="response")
emmeans(mixed, pairwise~Habitat|Season|Density, type="response")
emmip(mixed, ~Habitat|Season|Density, CIs=TRUE, CIarg=aes(lwd=1, color="black", linetype=1), type="response")

#predictions of effects
ggemmeans(mixed, terms=c("Opossum"), condition = c(Opossum=1, Season="Spring", Habitat="Bottomland"))

#predictions overall
ggemmeans(mixed, terms=c("Habitat"), condition = c(Opossum=4.3, Season="Spring", Density=75))
ggemmeans(mixed, terms=c("Habitat"), condition = c(Opossum=4.3, Season="Spring", Density=150))
ggemmeans(mixed, terms=c("Habitat"), condition = c(Opossum=5.6, Season="Fall", Density=75))
ggemmeans(mixed, terms=c("Habitat"), condition = c(Opossum=5.6, Season="Fall", Density=150))

write.csv(springraccoon, "springraccoon.csv") #these are used in figure creation
write.csv(springgrid, "springgrid.csv")
write.csv(newracc, "newracc.csv")
write.csv(newgrid, "newgrid.csv")




