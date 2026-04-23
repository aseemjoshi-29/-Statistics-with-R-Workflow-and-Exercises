# Project: Statistical Programming Self-Study
# Source: Field, A. (2012). Discovering Statistics Using R. Chapter 15.
# Objective: Nonparametric Testing in R.
# Date Completed: May, 2025


# Packages ------------------------------------------------------------

install.packages("clinfun")
install.packages("pgirmess")

library(clinfun)
library(pgirmess)
library(ggplot2)
library(pastecs)


# Wilcoxon rank-sum test --------------------------------------------

# Creating Dataframe
drug <- gl(2, 10, labels = c("Ecstasy", "Alcohol"))
sundayBDI <- c(15, 35, 16, 18, 19, 17, 27, 16, 13, 
               20, 16, 15, 20, 15, 16, 13, 14, 19, 18, 18)
wedsBDI <- c(28, 35, 35, 24, 39, 32, 27, 29, 36, 
             35, 5, 6, 30, 8, 9, 7, 6, 17, 3, 10)

drugData<-data.frame(drug, sundayBDI, wedsBDI)


# Creating the models
sunModel<-wilcox.test(sundayBDI ~ drug, data = drugData)


#uses normal approximation rather than exact p and removes continuity correction
wedModel<-wilcox.test(wedsBDI ~ drug, data = drugData, 
                      exact = FALSE, correct = FALSE)


# Custom function for effect size
rFromWilcox <- function(wilcoxModel, N){
  z<- qnorm(wilcoxModel$p.value/2)
  r<- z/ sqrt(N)
  cat(wilcoxModel$data.name, "Effect Size, r = " , r)
}


# Wilcoxon Signed Rank Test -------------------------------------------


# Running the analysis

alcoholModel <- wilcox.test(alcoholData$wedsBDI, alcoholData$sundayBDI, 
                          paired = TRUE, correct= FALSE)


ecstasyModel <- wilcox.test(ecstasyData$wedsBDI, ecstasyData$sundayBDI, 
                          paired = TRUE, correct= FALSE)


# effect size comes from rfromWilcox() custom function


# Kruskal Wallis --------------------------------------------------------

# Data Creation
soyaData<-read.csv("soya.csv", header = TRUE)
soyaData<-soyaData[, -c(1)]

soyaData$soya<-as.factor(soyaData$soya)

soyaData$soya<-factor(soyaData$soya, 
                      levels = levels(soyaData$soya)[c(4, 1, 2, 3)])


# Performing the Test

# Format:
# newModel <- kruskal.test(outcome ~ predictor, 
#                        data = dataFrame,
#                        na.action = "an.action")

kruskal.test(Sperm ~ Soya, data = soyaData)


#Interpreting test using ranks
soyaData$Ranks <- rank(soyaData$Sperm)


by(soyaData$Ranks, soyaData$Soya, mean)

# Post Hoc tests for Kruskal-Wallis
kruskalmc(Sperm ~ Soya, data = soyaData)


# Compare other groups to the first group (check factor ordering for this)
kruskalmc(Sperm ~ Soya, data = soyaData, cont = "two-tailed")




# Jonckheere test for trends --------------------------------------------

jonckheere.test(soyaData$Sperm, as.numeric(soyaData$Soya))
# grouping var needs to be numeric



# Friedman's ANOVA -----------------------------------------------------

friedman.test(as.matrix(diet))
#interpret chi square value and p-value

# Post Hoc

friedmanmc(as.matrix(diet))
# Brief Visualisation
spermPlot<-ggplot(soyaData, aes(soya, sperm))
spermPlot+geom_boxplot(outliers = TRUE)
