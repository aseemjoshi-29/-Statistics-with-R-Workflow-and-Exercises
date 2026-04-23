# Project: Statistical Programming Self-Study
# Source: Field, A. (2012). Discovering Statistics Using R. Chapter 9.
# Objective: Linear Regression and Bootstrapped Regression Modelling.
# Date Completed: April, 2025

# Packages ----------------------------------------------------------------

install.packages("compute.es")
install.packages("multcomp")

library(compute.es)
library(car)
library(ggplot2)
library(multcomp)
library(pastecs)
library(WRS)

# Data Creation ------------------------------------------------------------

superH<-read.csv("superhero.csv", header = T)
head(superH)

libido<-c(3,2,1,1,4,5,2,4,2,3,7,4,5,3,6)
dose<-gl(3,5, labels = c("Placebo", "Low Dose", "High Dose"))
viagraData<-data.frame(dose, libido)

viagPlot<-ggplot(viagraData, aes(dose, libido))
viagPlot+stat_summary(fun.y = mean, geom = "line", aes(group=1), colour="Blue", linetype = "dashed")+
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2)+
  labs(x= "Groups", Y = "Level of Libido")

by(viagraData$libido, viagraData$dose, stat.desc)
leveneTest(viagraData$libido, viagraData$dose, center = median)


# Performing ANOVA --------------------------------------------------------

viagModel<-aov(libido~dose, data = viagraData)
summary(viagModel)
plot(viagModel)

# Heterogenic Variances
oneway.test(libido~dose, data = viagraData)


# Robust Tests --------------------------------------------------------------

viagraWide<-unstack(viagraData, libido ~ dose)

t1way(viagraWide, tr = .1)
med1way(viagraWide)
t1waybt(viagraWide, tr = .2, nboot = 2000)
summary.lm(viagModel)


# Contrasts ---------------------------------------------------------------
contrast1<-c(-2,1,1)
contrast2<-c(0,-1,1)
contrasts(viagraData$dose)<-cbind(contrast1, contrast2)
viagraData$dose

viagPlanned<-aov(libido~dose, data = viagraData)
summary(viagPlanned)
summary.lm(viagPlanned)

#Trend Analysis ------------------------------------------------------------
contrasts(viagraData$dose)<-contr.poly(3)
viagraTrend<-aov(libido~dose, data = viagraData)
summary.lm(viagraTrend)


# Post Hoc Tests ------------------------------------------------------------

pairwise.t.test(viagraData$libido, viagraData$dose, p.adjust.method = "BH")

postHocs<-glht(viagModel, linfct = mcp(dose = "Dunnett"))
summary(postHocs)
confint(postHocs)

#Robust Post Hoc Tests

lincon(viagraWide)
mcppb20(viagraWide)

mes(2.2, 3.2, 1.3038405, 1.3038405, 5, 5)
mes(2.2, 5, 1.3038405, 1.5811388, 5, 5)
mes(3.2, 5, 1.3038405, 1.5811388, 5, 5)


# Effect Size --------------------------------------------------------------
rcontrast<-function(t, df)
{r<-sqrt(t^2/(t^2 + df))
print(paste("r = ", r))
}

rcontrast(2.474, 12)
rcontrast(2.029, 12)




# Test1 -----------------------------------------------------------------
teachData<-read.csv("teach_method.csv", header = T)
teachData<-teachData[, c("group", "exam")]
teachData$group<-as.factor(teachData$group)
teachData$group<-as.numeric(teachData$group)
teachData$group<-factor(teachData$group, levels = c(1:3), labels = c("Indifference", "Punish", "Reward"))
teachData$group

teachplot<-ggplot(teachData, aes(group, exam))
teachplot+stat_summary(fun.y = mean, geom = "line", aes(group=1), colour="Blue", linetype="dashed")+
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = .2)+
  stat_summary(fun.y = mean, geom = "point", colour="Blue")+
  labs(x = "Methods of Teaching", y = "Marks Obtained")

by(teachData$exam, teachData$group, stat.desc)
leveneTest(teachData$exam, teachData$group, center = median)

teachModel<-aov(exam ~ group, data = teachData)
summary(teachModel)
plot(teachModel)

oneway.test(exam ~ group, data = teachData)

contrastA<-c(1,-2,1)
contrastB<-c(-1,0,1)
contrasts(teachData$group)<-cbind(contrastA, contrastB)
teachData$group
teachPlanned<-aov(exam~group, data = teachData)
summary(teachPlanned)
summary.lm(teachPlanned)

contrasts(teachData$group)<-contr.poly(3)
teachTrend<-aov(exam~group, data = teachData)
summary.lm(teachTrend)

pairwise.t.test(teachData$exam, teachData$group, p.adjust.method = "BH")

teachHoc<-glht(teachModel, linfct = mcp(group = "Dunnett"))
summary(teachHoc)
confint(teachHoc)




# Practice 1 ------------------------------------------------------------
tumour<-read.csv("tumour.csv", header = TRUE)
tumour<-tumour[, c("usage", "tumour")]

tumourPlot<-ggplot(tumour, aes(usage, tumour))
tumourPlot + stat_summary(fun.y = mean, geom = "point") +
  stat_summary(fun.y = mean, geom = "line", aes(group=1), 
               colour = "blue", linetype = "dashed") +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar")

by(tumour$tumour, tumour$usage, stat.desc, basic = F, norm = T)
leveneTest(dataFin$tumour,dataFin$usage, center = median)

tumModel <- aov(tumour ~ usage, data = tumour)
summary(tumModel)

welshTumour <- oneway.test(tumour ~ usage, data = tumour)
welshTumour

summary.lm(tumModel)
contrasts(tumour$usage)<-contr.poly(6)
tumour$usage<-as.factor(tumour$usage)

tumourTrend<-aov(tumour ~ usage, data = tumour)
summary.lm(tumourTrend)

pairwise.t.test(tumour$tumour, tumour$usage, p.adjust.method = "BH")

tumourWide<-unstack(tumour, tumour ~ usage)
mcppb20(tumourWide)
lincon(tumourWide)
