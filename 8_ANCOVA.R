# Project: Statistical Programming Self-Study
# Source: Field, A. (2012). Discovering Statistics Using R. Chapter 11.
# Objective: Analysis of Covariance.
# Date Completed: May, 2025

# Packages ------------------------------------------------------------------

install.packages("effects")
library(car)
library(compute.es)
library(effects)
library(ggplot2)
library(multcomp)
library(pastecs)
library(WRS)


# Setting Up Data --------------------------------------------------------

libido<-c(3,2,5,2,2,2,7,2,4,7,5,3,4,4,7,5,4,9,2,6,3,4,4,4,6,4,6,2,8,5)
partnerLibido<-c(4,1,5,1,2,2,7,4,5,5,3,1,2,2,6,4,2,1,3,5,4,3,3,2,0,1,3,0,1,0)
dose<-c(rep(1,9),rep(2,8), rep(3,13))
dose<-factor(dose, levels = c(1:3), labels = c("Placebo", "Low Dose", "High Dose"))

viagraData<-data.frame(dose, libido, partnerLibido)


#Descriptive Statistics and Visualisation -----------------------------------

by(viagraData$libido, viagraData$dose, stat.desc)
by(viagraData$partnerLibido, viagraData$dose, stat.desc)


viagPlot1<-ggplot(viagraData, aes(dose, libido))
viagPlot1+geom_boxplot()
viagPlot1+geom_point()+
  geom_smooth(method = "lm", alpha = 0.1, colour = "Red", fill = "Green")
viagPlot1+stat_summary(fun.y = mean, geom = "line", aes(group=1), colour="Blue", linetype="dashed")+
  stat_summary(fun.data = mean_cl_normal, geom="errorbar", width = .2)+
  labs(x = "Dose of Viagra", y = "Libido of Participant")


viagPlot2<-ggplot(viagraData, aes(dose, partnerLibido))
viagPlot2+geom_boxplot()
viagPlot2+stat_summary(fun.y = mean, geom = "line", aes(group=1), colour="Blue", linetype="dashed")+
  stat_summary(fun.data = mean_cl_normal, geom="errorbar", width = .2)+
  labs(x = "Dose of Viagra", y = "Libido of Participant's Partner")


# Checking Assumptions
leveneTest(viagraData$libido, viagraData$dose, center = median)


# Running the ANCOVA model ----------------------------------------------

indepTest<-aov(partnerLibido ~ dose, data = viagraData)
summary(indepTest)

viagANCOVA<-aov(libido ~ dose + partnerLibido, data = viagraData)
summary(viagANCOVA)




# Practice2 ------------------------------------------------------------
invisibilityData<-read.csv("invisibility_base.csv", header = TRUE)
invisibilityData<-invisibilityData[,c("cloak", "mischief_pre", "mischief")]
invisibilityData$cloak<-as.factor(invisibilityData$cloak)
invisibilityData$cloak<-relevel(invisibilityData$cloak, "No cloak")
prePlot<-ggplot(invisibilityData, aes(cloak, mischief_pre))
prePlot+geom_boxplot()
mischiefPlot<-ggplot(invisibilityData, aes(cloak, mischief))
mischiefPlot+geom_boxplot()

by(invisibilityData$mischief_pre, invisibilityData$cloak, stat.desc, basic = F, norm = T)
by(invisibilityData$mischief, invisibilityData$cloak, stat.desc, basic = F, norm = T)

noCloak<-subset(invisibilityData, cloak=="No cloak")
invisCloak<-subset(invisibilityData, cloak=="Cloak")

ancboot(invisCloak$mischief_pre, invisCloak$mischief, noCloak$mischief_pre, noCloak$mischief, nboot = 2000)
ancova(invisCloak$mischief_pre, invisCloak$mischief, noCloak$mischief_pre, noCloak$mischief)
