# Project: Statistical Programming Self-Study
# Source: Field, A. (2012). Discovering Statistics Using R. Chapter 16.
# Objective: Multivariate Analysis of Variance (MANOVA)
# Date Completed: May, 2025


# Packages ----------------------------------------------------

install.packages("mvoutlier")
install.packages("mvnormtest")

library(car)
library(ggplot2)
library(MASS)
library(mvoutlier)
library(mvnormtest)
library(pastecs)
library(reshape)
library(WRS)


# Data Importing ------------------------------------------------


# Creating coding variable
Group<-gl(3, 10, labels = c("CBT", "BT", "NT"))

# Creating vars
Actions<-c(5, 5, 4, 4, 5, 3, 7, 6, 6, 4, 4, 4, 1, 1, 4, 6, 
           5, 5, 2, 5, 4, 5, 5, 4, 6, 4, 7, 4, 6, 5)
Thoughts<-c(14, 11, 16, 13, 12, 14, 12, 15, 16, 11, 14, 15, 13, 14, 15, 19, 
            13, 18, 14, 17, 13, 15, 14, 14, 13, 20, 13, 16, 14, 18)

ocdData <- data.frame(Actions, Thoughts, Group)

ocdData$Group<-factor(ocdData$Group, 
                      levels = c("CBT", "BT", "No Treatment Control"), 
                      labels = c("CBT", "BT", "NT"))


# Descriptive Statistics ----------------------------------------

# Obtaining Variance Covariance Matrix
by(ocdData[, 1:2], ocdData$Group, cov)

cbt <- t(ocdData[1:10, 1:2])
bt <- t(ocdData[11:20, 1:2])
nt <- t(ocdData[21:30, 1:2])

# checking for normality
map(c(cbt, bt, nt), mshapiro.test)


# looking for multivariate outliers
aq.plot(ocdData[, 1:2])


# Setting Contrasts ------------------------------------------------------

# setting NT group as baseline
contrasts(ocdData$Group) <- contr.treatment(3, base = 3)

# individual contrasts
CBT_vs_NT <- c(1, 0, 0)
BT_vs_NT <- c(0, 1, 0)
contrasts(ocdData$Group) <- cbind(CBT_vs_NT, BT_vs_NT)



# Creating the MANOVA model ---------------------------------------------

# Format :
# newModel <- manova(outcome ~ predictor(s), 
#                  data = dataFrame,
#                  na.action = an action))

# Defining multiple dependent variables
outcome <- cbind(ocdData$Actions, ocdData$Thoughts)

# Running the model variable
ocdModel <- manova(outcome ~ Group, data = ocdData)

summary.aov(ocdModel)

# Obtaining several test statistics
summary(ocdModel, intercept = T) # Pillai's trace
summary(ocdModel, intercept = T, test = "Wilks")
summary(ocdModel, intercept = T, test = "Hotelling")
summary(ocdModel, intercept = T, test = "Roy")


# Usually there is no need to implement contrast analysis as two variables are related


# Robust MANOVA ------------------------------------------------------

# Formatting data
ocdData$row<-rep(1:10, 3)

ocdMelt <- melt(ocdData, 
              id = c("Group", "row"), 
              measured = c("Actions", "Thoughts"))


names(ocdMelt)<-c("Group", "row", "Outcome_Measure", "Frequency")

# Casting back to achieve formatted dataset
ocdRobust <- cast(ocdMelt, 
                  row ~ Group + Outcome_Measure, 
                  value = "Frequency")

ocdRobust$row <- NULL


# MANOVA using (Munzel & Brunner, 2000)'s method
#mulrank(number of groups, numer of dependent variables, data)
mulrank(3, 2, ocdRobust)


#MANOVA using Choi and Marden’s (1997) method
cmanova(3, 2, ocdRobust)



# Post Hoc Discriminant Analysis ----------------------------------------

# Format:
# newModel<-lda(Group ~ Predictor(s),
#               data = dataFrame,
#               prior = prior probabilities,
#               na.action = "na.omit")

ocdDFA <- lda(Group ~ Actions + Thoughts, data = ocdData)


# Obtaining discriminant scores
predict(ocdDFA)

# plotting scores divided by group membership
plot(ocdDFA)