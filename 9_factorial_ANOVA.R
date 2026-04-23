# Project: Statistical Programming Self-Study
# Source: Field, A. (2012). Discovering Statistics Using R. Chapter 12.
# Objective: Factorial ANOVA.
# Date Completed: May, 2025


# Packages ---------------------------------------------------------------

library(car)
library(compute.es)
library(ggplot2)
library(multcomp)
library(pastecs)
library(reshape)
library(WRS)


# Data Creation -------------------------------------------------------

goggles<-read.csv("goggles.csv", header = T)

goggles<-goggles[, c("alcohol", "attractiveness")]

#Another way to convert chars into factor
gender<-gl(2, 24, labels = c("Female", "Male"))
goggles$gender<-gender


#Re-leveling factors
goggles$gender<-relevel(goggles$gender, "Male")


goggles$alcohol<-factor(goggles$alcohol, 
                        levels = c("Placebo", "Low dose", "High dose"), 
                        labels = c("Placebo", "Low dose", "High dose"))

levels(goggles$alcohol)


# Visualization --------------------------------------------------------

linePlot<-ggplot(goggles, aes(alcohol, attractiveness))

linePlot+ stat_summary(fun.y = mean, geom = "point")+
  stat_summary(fun.y = mean, geom = "line", aes(group=gender))+
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = .2)

goggleBox<-ggplot(goggles, aes(alcohol, attractiveness))

goggleBox+geom_boxplot()+
  facet_wrap( ~ gender)


#Evaluating heterogeneity of variances
leveneTest(goggles$attractiveness, goggles$gender, center = median)
leveneTest(goggles$attractiveness, goggles$alcohol, center = 
             median)


# Setting Contrasts --------------------------------------------------

contrasts(goggles$alcohol) <- cbind(c(-2, 1, 1), c(0, -1, 1))
contrasts(goggles$gender) <- c(-1, 1)


# Creating Factorial ANOVA Model -------------------------------------

#similar to anova function aov()
gogglesModel<-aov(attractiveness ~ alcohol*gender, data = gogglesData)


# deriving type 3 sum of squares for the model
Anova(gogglesModel, type="III")


# viewing contrast wise analysis
summary.lm(gogglesModel)


# Robust Factorial ANOVA ---------------------------------------------

#Creating Molten and Wide Data
goggles$row<-rep(1:8, 6)

gogglesMolten<-melt(goggles, 
                    id = c("row", "gender", "alcohol"), 
                    measured = c("attractiveness"))
head(gogglesMolten)

gogglesWide<-cast(gogglesMolten, row ~ gender + alcohol)
head(gogglesWide)



# Robust Factorial

# 2-way indep anova on trimmed means
# t2way(levels of factor A, levels of factor B, data, tr = .2, alpha = .05)
t2way(2,3, gogglesWide)

#poc hocs for the earlier function
pbad2way(2,3, gogglesWide)


# 2-way indep anova using m-measures
mcp2atm(2,3, gogglesWide)

# post hoc tests for the earlier function
mcp2a(2,3, gogglesWide)


# Function for calculating Effect Sizes ------------------------------------


omega_factorial<-function(n, a, b, SSa, SSb, SSab, SSr)
{
  MSa<-SSa/(a-1)
  MSb<-SSb/(b-1)
  MSab<-SSab/((a-1)*(b-1))
  MSr<-SSr/(a*b*(n-1))
  varA<-((a-1)*(MSa-MSr))/(n*a*b)
  varB<-((b-1)*(MSb-MSr))/(n*a*b)
  varAB<-((a-1)*(b-1)*(MSab-MSr))/(n*a*b)
  varTotal<-varA + varB + varAB + MSr
  print(paste("Omega-Squared A: ", varA/varTotal))
  print(paste("Omega-Squared B: ", varB/varTotal))
  print(paste("Omega-Squared AB: ", varAB/varTotal))
}
