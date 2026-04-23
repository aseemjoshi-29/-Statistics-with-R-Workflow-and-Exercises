# Project: Statistical Programming Self-Study
# Source: Field, A. (2012). Discovering Statistics Using R. Chapter 17.
# Objective: Exploratory Factor Analysis.
# Date Completed: May, 2025


# Packages -------------------------------------------------------------

install.packages("corpcor")
install.packages("GPArotation")
install.packages("DSUR")

library(corpcor)
library(ggplot2)
library(GPArotation)
library(psych)


# Data Creation -----------------------------------------------------------

raqData<-read.csv("raq.csv", header = T)
raqData<-raqData[, -c(1)]

# calculating correlation matrix
corrMatrix<-cor(raqData)
round(corrMatrix, 2)

# Bartlett's test and KMO
cortest.bartlett(corrMatrix, n = 2571)

kmo(raqData)

# Finding determinant of correlation matrix
det(corrMatrix)


# Factor Extraction ------------------------------------------------------

# Format:
# pcModel<-principal(dataframe/R-matrix, 
#                    nfactors = number of factors,
#                    rotate = "method of rotation",
#                    scores = TRUE/FALSE)


pc1<-principal(corrMatrix, nfactors = 23, rotate = "none")
pc1


# Scree-plot of the PCA values
plot(pc1$values, type = "b")

# Updated model
pc2<-principal(corrMatrix, nfactors = 4, rotate = "none")
pc2

# obtaining reproduced correlations to evaluate optimal number of factors
factor.model(pc2$loadings)


# differences between actual and reproduced correlations is residuals
residuals <- factor.residuals(corrMatrix, pc2$loadings)
corrMatrix

# only extracting elements above the diagonal
residuals <- as.matrix(residuals[upper.tri(residuals)])
residuals


# evaluating number of large residuals
sum(abs(residuals) > 0.05)

# proportion of number of residuals
sum(abs(residuals) > 0.05) / nrow(residuals)

# root mean square residuals
sqrt(mean(residuals^2))



# Factor Rotation ------------------------------------------------------

# Varimax
pc3 <-  principal(raqData, nfactors = 4, rotate = "varimax")


# Interpreting factor loading matrix, showing only loadings above 0.3 and sorting
print.psych(pc3, cut = 0.3, sort = TRUE)


# Oblique Rotation
pc4 <- principal(raqData, nfactors = 4, rotate = "oblimin")

print.psych(pc4, cut = 0.3, sort = TRUE)


# Obtaining structure loadings by multiplying matrices
pc4$loadings %*% pc4$Phi

DSUR::factor.structure(pcModel, cut = 0.3, decimals = 2) # alternative


# Factor Scores ----------------------------------------------------

pc5 <- principal(raqData, nfactors = 4, rotate = "oblimin", scores = TRUE)

head(pc5$scores, 15)

# combining factor scores with data
raqData <- cbind(raqData, pc5$scores)




# Reliability Analysis in R -------------------------------------------

computerFear<-raqData[, c(6, 7, 10, 13, 14, 15, 18)]
statisticsFear <- raqData[, c(1, 3, 4, 5, 12, 16, 20, 21)]
mathFear <- raqData[, c(8, 11, 17)]
peerEvaluation <- raqData[, c(2, 9, 19, 22, 23)]


# cronbach's alpha function needs to be keyed for reverse scoring

psych::alpha(statisticsFear, keys = c(1, -1, 1, 1, 1, 1, 1, 1))


# IRIS  Practice -------------------------------------------------------
data("iris")
head(iris)

# Scale the data 
iris_scaled <- scale(iris[,1:4]) 
head(iris_scaled)


# Perform factor analysis 
fa <- fa(r = iris_scaled, 
         nfactors = 4, 
         rotate = "varimax") 
summary(fa) 


# View the factor loadings
fa$loadings 


# examine factor structure for 
# different subsets of the data 
subset1 <- subset(iris[,1:4], 
                  iris$Sepal.Length < mean(iris$Sepal.Length))
fa1 <- fa(subset1, nfactors = 4,
          rotate = "none") 
print(fa1) 


subset2 <- subset(iris[,1:4], 
                  iris$Sepal.Length >= mean(iris$Sepal.Length)) 
fa2 <- fa(subset2, nfactors = 4,
          rotate = "none")
print(fa2) 


# display variance explained by each factor 
print(fa$Vaccounted, rotate = "none")



# BFI DATA ------------------------------------------

bfi_data=bfi

#Remove rows with missing values and keep only complete cases
bfi_data=bfi_data[complete.cases(bfi_data),]

#calculating the correlation
bfi_corr <- cor(bfi_data)

#Factor analysis of the data
factors_data <- fa(r = bfi_corr, nfactors = 6)
factors_data

factors_data$Phi
factors_data$chi
