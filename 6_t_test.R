# Project: Statistical Programming Self-Study
# Source: Field, A. (2012). Discovering Statistics Using R. Chapter 8.
# Objective: Linear Regression and Bootstrapped Regression Modelling.
# Date Completed: April, 2025


# Libraries -------------
install.packages("pastecs")
install.packages("WRS")

library(ggplot2)
library(pastecs)
library(WRS)
library(reshape)
library(discovr)

# Setup -------------
setwd("C:/Users/Admin/Desktop/Important Documents/R/Datasets")

# Data Entry & Visualization -------------
participant <- c(1:24)
group <- c(rep("Picture", 12), rep("Real Spider", 12))
anxiety <- c(30, 35, 45, 40, 50, 35, 55, 25, 30, 45, 40, 50, 40, 35, 50, 55, 65, 55, 50, 35, 30, 50, 60, 39)
spiderLong <- data.frame(participant, group, anxiety)

spiderLong$group <- as.factor(spiderLong$group)

spiderPlot <- ggplot(spiderLong, aes(group, anxiety))
spiderPlot + stat_summary(fun.y = mean, geom = "bar") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2) +
  labs(x = "Groups", y = "Mean Anxiety")

pictureAnx <- spiderLong[group == "Picture", "anxiety"]
realAnx <- spiderLong[group == "Real Spider", "anxiety"]
spiderWide <- data.frame(pictureAnx, realAnx)

# Correcting Repeated Measures Designs -------------
spiderWide$pMean <- (spiderWide$pictureAnx + spiderWide$realAnx) / 2
grandMean <- mean(c(spiderWide$pictureAnx, spiderWide$realAnx))
spiderWide$adj <- grandMean - spiderWide$pMean

spiderWide$pic_adj <- spiderWide$pictureAnx + spiderWide$adj
spiderWide$real_adj <- spiderWide$realAnx + spiderWide$adj

spiderWide2 <- melt.data.frame(spiderWide, id.vars = c("realAnx", "pictureAnx", "pMean", "adj"), measure.vars = c("real_adj", "pic_adj"))

# Function for Correcting RM Design -------------
rmMeanAdjust <- function(dataframe) {
  varNames <- names(dataframe)
  pMean <- (dataframe[, 1] + dataframe[, 2]) / 2
  grandmean <- mean(c(dataframe[, 1], dataframe[, 2]))
  adj <- grandmean - pMean
  varA_adj <- dataframe[, 1] + adj
  varB_adj <- dataframe[, 2] + adj
  output <- data.frame(varA_adj, varB_adj)
  names(output) <- c(paste(varNames[1], "Adj", sep = "_"), paste(varNames[2], "_Adj", sep = "_"))
  return(output)
}

# Custom T-Test Function from Means -------------
ttestfromMeans <- function(x1, x2, sd1, sd2, n1, n2) {
  df <- n1 + n2 - 2
  poolvar <- (((n1 - 1) * sd1^2) + ((n2 - 1) * sd2^2)) / df
  t <- (x1 - x2) / sqrt(poolvar * ((1 / n1) + (1 / n2)))
  sig <- 2 * (1 - (pt(abs(t), df)))
  paste("t(df = ", df, ") = ", t, ", p = ", sig, sep = "")
}

# Independent T-Tests & Robust Methods -------------
by(spiderLong$anxiety, spiderLong$group, stat.desc, basic = F, norm = T)

ind.t.test <- t.test(anxiety ~ group, data = spiderLong)
yuen(spiderWide$realAnx, spiderWide$pictureAnx)
yuenbt(spiderWide$realAnx, spiderWide$pictureAnx, nboot = 2000)

# Dependent T-Test & Effect Sizes -------------
dep.t.test <- t.test(spiderWide$realAnx, spiderWide$pictureAnx, paired = T)

t = dep.t.test$statistic[[1]]
df = dep.t.test$parameter[[1]]
r <- sqrt((t^2) / (t^2 + df))
round(r, 3)