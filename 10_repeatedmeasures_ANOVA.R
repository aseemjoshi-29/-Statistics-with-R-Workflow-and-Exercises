# Project: Statistical Programming Self-Study
# Source: Field, A. (2012). Discovering Statistics Using R. Chapter 13.
# Objective: Repeated-Measures ANOVA.
# Date Completed: May, 2025


# Packages --------------------------------------------------------------

install.packages("ez")
install.packages("nlme")

library(ez)
library(ggplot2)
library(nlme)
library(multcomp)
library(pastecs)
library(reshape)
library(WRS)


# Data Creation ---------------------------------------------------------------

participant<-gl(8,4,labels = c("P1", "P2", "P3", "P4", "P5", "P6", "P7", "P8" ))

Animal<-gl(4, 1, 32, labels = c("Stick Insect", "Kangaroo Testicle", "Fish Eye", "Witchetty Grub"))

Retch<-c(8, 7, 1, 6, 9, 5, 2, 5, 6, 2, 3, 8, 5, 3, 1, 9, 8, 4, 5, 8, 7, 5, 6, 7, 10, 2, 7, 2, 12, 6, 8, 1)

longBush<-data.frame(participant, Animal, Retch)


# Visualisation ------------------------------------------------------------

retchBar<-ggplot(longBush, aes(Animal, Retch))
retchBar+stat_summary(fun.y = mean, geom = "bar", fill = "white", colour = "black")+
  stat_summary(fun.y = mean, geom = "point")+
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0)

retchBar+geom_boxplot()


# Choosing Contrasts --------------------------------------------------------


PartvsWhole<-c(1, -1, -1, 1)
TesticlevsEye<-c(0, -1, 1, 0)
StickvsGrub<-c(-1, 0, 0, 1)

contrasts(longBush$Animal)<-cbind(PartvsWhole, TesticlevsEye, StickvsGrub)


# Method 1 to do RM-ANOVA --------------------------------------------------

# newModel<-ezANOVA(data = dataframe, 
#                  dv = .(outcome variable), 
#                  wid = .(variable identifying participants),  
#                  within = .(repeated measures predictors), 
#                  between = .(between-group predictors), 
#                  detailed = FALSE, 
#                  type = 2)


# Bonferroni corrected post-hoc tests

pairwise.t.test(longBush$Retch, longBush$Animal, paired = TRUE, p.adjust,
                method = "bonferroni")




# Multi-level Method for RM-ANOVA -------------------------------------

# Function Format
# newModel <-lme(outcome ~ predictor(s), random = random effects, 
#               data = dataframe, method = "ML")

bushModel<-lme(Retch ~ Animal, random = ~1|Participant/Animal, 
               data = longBush, method = "ML")


# checking only for Predictor effect
baseline<-lme(Retch ~ 1, random = ~1|Participant/Animal, data = longBush, 
              method = "ML") # created baseline model with only intercept


anova(baseline, bushModel)


# Post Hoc tests for Multilevel Model

postHocs<-glht(bushModel, linfct = mcp(Animal = "Tukey"))

summary(postHocs)
confint(postHocs) 


# Robust Repeated Measures ANOVA -----------------------------------------

bushData2<-bushData[, -c(1)]


# 1-way RM-ANOVA on trimmed means
rmanova(bushData2, tr = .2)

# post hoc for above function
rmmcp(bushData2)

# 1-way RM-ANOVA using bootstrap
rmanovab(bushData2, nboot = 2000)

# post hoc for bootstrapped rm anova
pairdepb(bushData2, nboot = 2000)


# Effect sizes are the same as t-tests


# Factorial Repeated Measures Designs ----------------------------------------------

beerpos<-c(1, 43, 15, 40, 8, 17, 30, 34, 34, 26, 1, 7, 22, 30, 40, 15, 20, 9, 14, 15)
beerneg<-c(6, 30, 15, 30, 12, 17, 21, 23, 20, 27, -19, -18, -8, -6, -6, -9, -17, -12, -11, -6)
beerneut<-c(5, 8, 12, 19, 8, 15, 21, 28, 26, 27, -10, 6, 4, 3, 0, 4, 9, -5, 7, 13)
winepos<-c(38, 20, 20, 28, 11, 17, 15, 27, 24, 23, 28, 26, 34, 32, 24, 29, 30, 24, 34, 23)
wineneg<-c(-5, -12, -15, -4, -2, -6, -2, -7, -10, -15, -13, -16, -23, -22, -9, -18, -17, -15, -14, -15)
wineneut<-c(4, 4, 6, 0, 6, 6, 16, 7, 12, 14, 13, 19, 14, 21, 19, 7, 12, 18, 20, 15)
waterpos<-c(10, 9, 6, 20, 27, 9, 19, 12, 12, 21, 33, 23, 21, 17, 15, 13, 16, 17, 19, 29)
waterneg<-c(-14, -10, -16, -10, 5, -6, -20, -12, -9, -6, -2, -17, -19, -11, -10, -17, -4, -4, -1, -1)
waterneut<-c(-2, -13, 1, 2, -5, -13, 3, 2, 4, 0, 9, 5, 0, 4, 2, 8, 10, 8, 12, 10)



attitudeData<-data.frame(beerpos, beerneg, beerneut, 
                         winepos, wineneg, wineneut, 
                         waterpos, waterneg, waterneut)

head(attitudeData)

attitudeData$participants<-gl(20, 1, 
                              labels = c("p1", "p2", "p3", "p4", "p5", 
                                         "p6", "p7", "p8", "p9", "p10", 
                                         "p11", "p12", "p13", "p14", "p15", 
                                         "p16", "p17", "p18", "p19", "p20"))


# Melting Dataframe
longAttitude <-melt(attitudeData, 
                    id = "participants", 
                    measured = c( "beerpos", "beerneg", "beerneut", 
                                  "winepos", "wineneg", "wineneut", 
                                  "waterpos", "waterneg", "waterneut"))

names(longAttitude)<-c("participant", "groups", "attitude")


longAttitude$drink<-gl(3, 60, labels = c("Beer", "Wine", "Water"))

longAttitude$imagery<-gl(3, 20, 180, 
                         labels = c("Positive", "Negative", "Neutral"))


# Visualization_2 Self Test -------------------------------------------------------

drinkPlot<-ggplot(longAttitude, aes(drink, attitude, colour = imagery))
drinkPlot+geom_boxplot(outliers = TRUE)+
  facet_wrap( ~ imagery)

drinkPlot+stat_summary(fun.y = mean, geom = "bar", fill = "white", colour = "black")+
  stat_summary(fun.y = mean, geom = "point")+
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0)

drinkPlot+stat_summary(fun.y = mean, geom = "line", aes(group=imagery))+
  stat_summary(fun.y = mean, geom = "point")+
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = .2)


# Setting Contrasts ------------------------------------------------------

# Drink Contrasts
AlcoholvsWater<-c(1, 1, -2)
BeervsWine<-c(-1, 1, 0)

contrasts(longAttitude$drink)<-cbind(AlcoholvsWater, BeervsWine)


#Imagery Contrasts
NegativevsOther <- c(1, -2, 1)
PositivevsNeutral <- c(-1, 0, 1)

contrasts(longAttitude$imagery)<-cbind(NegativevsOther, PositivevsNeutral)


# Factorial RM-ANOVA --------------------------------------------

attitudeModel <- ezANOVA(data = longAttitude, 
                       dv = .(attitude), 
                       wid = .(participant),  
                       within = .(imagery, drink), 
                       type = 3, 
                       detailed = TRUE)

attitudeModel


# Post Hoc tests
pairwise.t.test(longAttitude$attitude, longAttitude$groups, paired = TRUE, 
                p.adjust.method = "bonferroni")


# Factorial RM models can also be compared to a baseline
baseline <- lme(attitude ~ 1, random = ~1|participant/drink/imagery, data 
              = longAttitude, method = "ML")


# They can also be compared to effects of each indiv predictor factor

drinkMod <- update(baseline, .~. + drink)
imageryMod <- update(drinkModel, .~. + imagery)
attitudeMod<-update(imageryModel, .~. + drink:imagery)

anova(baseline, drinkMod, imageryMod, attitudeMod)


# Effect sizes are also the same as t-tests