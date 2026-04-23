# Project: Statistical Programming Self-Study
# Source: Field, A. (2012). Discovering Statistics Using R. Chapter 14.
# Objective: Mixed Designs ANOVA
# Date Completed: May, 2025


# Packages -----------------------------------------------------------------

library(ez)
library(ggplot2)
library(nlme)
library(multcomp)
library(pastecs)
library(reshape)
library(WRS)


# Data Creation --------------------------------------------------------------

dateData<-read.csv("looks_tidy.csv", header = TRUE)
dateData<-dateData[, -c(1:2)]

dateData$groups<-gl(9, 10, labels = c("att_high", "av_high", "ug_high", 
                                      "att_some", "av_some", "ug_some", 
                                      "att_none", "av_none", "ug_none"))


dateData$attractiveness<-factor(dateData$attractiveness, 
                                levels = c("Attractive", "Average", "Ugly"), 
                                labels = c("Attractive", "Average", "Ugly"))


dateData$charisma<-factor(dateData$charisma, 
                          levels = c("Highly charismatic", "Some charisma", "Dullard"), 
                          labels = c("Highly charismatic", "Some charisma", "Dullard"))



dateData$gender <- gl(2, 10, 180, labels = c("Male", "Female"))

dateData$participants <- gl(20, 1, labels = c("p1", "p2", "p3", "p4", "p5", 
                                            "p6", "p7", "p8", "p9", "p10", 
                                            "p11", "p12", "p13", "p14", "p15", 
                                            "p16", "p17", "p18", "p19", "p20"))


# Visualisation Tests -------------------------------------------------------

# Indiv Variable Plots
speedPlots <- ggplot(dateData, aes(attractiveness, rating, colour = charisma))
speedPlots + geom_boxplot(outliers = TRUE)+
  facet_wrap( ~ gender)


genderPlot <- ggplot(dateData, aes(charisma, rating))
genderPlot + stat_summary(fun.y = mean, geom = "bar", fill = "white", colour = "black")+
  stat_summary(fun.y = mean, geom = "point")+
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0)


# Dual Var Plots
looksGenPlot <- ggplot(dateData, aes(attractiveness, rating, colour = gender))
looksGenPlot + stat_summary(fun.y = mean, geom = "line", aes(group=gender))+
  stat_summary(fun.y = mean, geom = "point")+
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = .2)


persGenPlot <- ggplot(dateData, aes(charisma, rating, colour = gender))
persGenPlot + stat_summary(fun.y = mean, geom = "line", aes(group=gender))+
  stat_summary(fun.y = mean, geom = "point")+
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = .2)


looksPersPlot <- ggplot(dateData, aes(attractiveness, rating, colour = charisma))
looksPersPlot + stat_summary(fun.y = mean, geom = "line", aes(group=charisma))+
  stat_summary(fun.y = mean, geom = "point")+
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = .2)


# All Vars Plot
threeWayPlot <- ggplot(dateData, aes(attractiveness, rating, colour = charisma))

threeWayPlot + stat_summary(fun.y = mean, geom = "line", aes(group=charisma))+
  stat_summary(fun.y = mean, geom = "point")+
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = .2)+
  facet_wrap( ~ gender)



# Contrasts --------------------------------------------------------

# Personality Contrasts
SomevsNone<-c(1, 1, -2)
HivsAv<-c(1, -1, 0)

contrasts(dateData$charisma) <- cbind(SomevsNone, HivsAv)
dateData$charisma


# Attractiveness Contrasts
AttractivevsUgly<-c(1, 1, -2)
AttractvsAv<-c(1, -1, 0)

contrasts(dateData$attractiveness) <- cbind(AttractivevsUgly, AttractvsAv)
dateData$attractiveness


# Running the Mixed-Design ANOVA model -------------------------------------

speedModel<-ezANOVA(data = speedData, 
                    dv = .(dateRating), 
                    wid =  .(participant),  
                    between = .(gender), 
                    within = .(looks, personality), 
                    type = 3, 
                    detailed = TRUE)



# Mixed Design ANOVA can be run as GLM in the same way as RM-ANOVA
# Create Baseline, Indiv Var Models, Dual Var Interactions and Triple Interaction


# Effect Sizes same formula as RM-ANOVA



# Robust Mixed Design ANOVA --------------------------------------------

pictureData<-read.csv("profile_pic.csv", header = TRUE)
pictureData<-pictureData[, -c(1)]


pictureData$rel_status<-factor(pictureData$rel_status, 
                               levels = c("Single", "In a relationship"), 
                               labels = c("Single", "In a relationship"))


pictureData$profile_pic<-factor(pictureData$profile_pic, 
                                levels = c("Alone", "Couple"),
                                labels = c("Alone", "Couple"))


# Creating  a factor
one<-rep(c(1:17), each = 2)
two<-rep(c(1:23), each = 2)
pictureData$diff <- c(one, two)


pictureData$diff <- factor(pictureData$diff, 
                         levels = c(1:2), 
                         labels = c("Couple", "Alone"))

pictureMolten <- melt(pictureData, 
                    id = c("rel_status", "requests"), 
                    measured = c("profile_pic"))


pictureWide <- cast(pictureData, diff ~ rel_status + profile_pic, value = "requests")
pictureWide <- pictureWide[, c(5,4,3,2)]


# trimmed means
tsplit(2, 2, pictureWide)

# main effect of factor A using m-measures and bootstrap
sppba(2, 2, pictureWide, est = mom, nboot = 2000)

# main effect of factor A using m-measures and bootstrap
sppbb(2, 2, pictureWide, est = mom, nboot = 2000)

# a x b interaction using m-measures and bootstrap
sppbi(2, 2, pictureWide, est = mom, nboot = 2000)


# Practice ------------------------------------------------------

relPLot <- ggplot(pictureData, aes(rel_status, requests, colour = profile_pic))
relPLot + stat_summary(fun.y = mean, geom = "line", aes(group=profile_pic))+
  stat_summary(fun.y = mean, geom = "point")+
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = .2)
