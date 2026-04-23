# Project: Statistical Programming Self-Study
# Source: Field, A. (2012). Discovering Statistics Using R. Chapter 18.
# Objective: Chi Square and Log-Linear Analysis
# Date Completed: May, 2025

# Packages ----------------------------------------------------
install.packages("gmodels")

library(gmodels)
library(MASS)

# Dataset Creation -------------------------------------------------

food <- c(10, 28)
affection <- c(114, 48)
catsTable <- cbind(food, affection)


# Running Chi-Square Analysis ----------------------------------------

# CrossTable(predictor, outcome, fisher = TRUE, chisq = TRUE, expected = TRUE, 
#            sresid = TRUE, format = "SAS"/"SPSS")


CrossTable(catsData$Training, catsData$Dance, fisher = TRUE, chisq = TRUE, 
           expected = TRUE, sresid = TRUE, format = "SPSS")


# if Std R > ±1.96, significant at p < .05, 
# Std R > ±2.58, significant at p < .01
# Std R > ±3.29, significant at p < .001.


# Loglinear Analysis -----------------------------------------------------

# Reading in the data
catsDogs<-read.delim("CatsandDogs.dat", header = TRUE)

# Subsetting
justCats = subset(catsDogs, Animal=="Cat")
justDogs = subset(catsDogs, Animal=="Dog")

dog_tab <- CrossTable(justCats$Training, 
                      justCats$Dance, 
                      sresid = TRUE, 
                      prop.t = FALSE, 
                      prop.c = FALSE, 
                      prop.chisq = FALSE, 
                      format = "SPSS")


cat_tab <- CrossTable(justDogs$Training, 
                      justDogs$Dance, 
                      sresid = TRUE, 
                      prop.t = FALSE, 
                      prop.c = FALSE, 
                      prop.chisq = FALSE, 
                      format = "SPSS")


# Individual oglinear analysis as chi-square
# Step 1 : Create contingency tables

catTable <- xtabs(~ Training + Dance, data = justCats)


# Step 2: Create loglm() model
catSaturated<-loglm(~ Training * Dance, 
                    data = catTable, 
                    fit = TRUE)


# Step 3 : Create Interaction-free model
catNoInteraction <- loglm(~ Training + Dance, data = catTable, fit = TRUE)


# Step 4 : Create Mosaic Plot
mosaicplot(catSaturated$fit, shade = TRUE, main = "Cats: Saturated Model")

mosaicplot(catNoInteraction$fit, shade = TRUE, main = "Cats: Expected")


# Log-linear Analysis with 2+ Vars -------------------------------------------

# Creating contingency table
CatDogContingencyTable <- xtabs(~ Animal + Training + Dance, data = catsDogs)


# Create Model
caturated <- loglm(~ Animal*Training*Dance, data = CatDogContingencyTable)
summary(caturated)

# Model without three-way interaction:
threeWay <- update(caturated, .~. -Animal:Training:Dance)
summary(threeWay)


# EXTRA : removing specific interactions to compare with model

trainingDance <- update(threeWay, .~. -Training:Dance)
animalDance <- update(threeWay, .~. -Animal:Dance)
animalTraining <- update(threeWay, .~. -Animal:Training)

anova(threeWay, trainingDance)
anova(threeWay, animalDance)
anova(threeWay, animalTraining)


# Creating mosaic plot of 2+ Vars

mosaicplot(CatDogContingencyTable, shade = TRUE, main = "Cats and Dogs")