# Project: Statistical Programming Self-Study
# Source: Field, A. (2012). Discovering Statistics Using R. Chapter 7.
# Objective: Linear Regression and Bootstrapped Regression Modelling.
# Date Completed: April, 2025

# Packages -------------
install.packages("car")
install.packages("QuantPsyc")

library(boot)
library(car)
library(QuantPsyc)
library(ggplot2)
library(dplyr)

# Setup -------------
setwd("C:/Users/Admin/Desktop/Important Documents/R/Datasets")

# Simple Regression -------------
album1 <- read.csv("album_sales.csv", header = T)
albumSales.1 <- lm(album1$sales ~ album1$adverts, na.action = na.exclude)
summary(albumSales.1)
sqrt(0.3346)

# Influence & Case Diagnostics -------------
betaData <- read.csv("df_beta.csv", header = T)
beta.1 <- lm(betaData$x ~ betaData$y)
summary(beta.1)

betaRemoved <- lm(betaData$x[betaData$case != 30] ~ betaData$y[betaData$case != 30])
summary(betaRemoved)

# Multiple Regression Models -------------
albumSales.2 <- lm(sales ~ adverts, data = album1)
summary(albumSales.2)

albumSales.3 <- lm(sales ~ adverts + airplay + image, data = album1)
summary(albumSales.3)

lm.beta(albumSales.3)
confint(albumSales.3)

anova(albumSales.2, albumSales.3)

album2 <- album1[, c("adverts", "sales", "airplay", "image")]

# Diagnostic Tests: Outliers & Influence -------------
album2$residuals <- resid(albumSales.3)
album2$standardized.residuals <- rstandard(albumSales.3)
album2$studentized.residuals <- rstudent(albumSales.3)

album2$cooks.distance <- cooks.distance(albumSales.3)
album2$dfbeta <- dfbeta(albumSales.3)
album2$dffit <- dffits(albumSales.3)
album2$leverage <- hatvalues(albumSales.3)
album2$covariance.ratios <- covratio(albumSales.3)

round(album2, digits = 3)
head(album2)

album2$large.residual <- album2$standardized.residuals > 2 | album2$standardized.residuals < -2
sum(album2$large.residual)

album2[album2$large.residual, c("sales", "airplay", "image", "adverts", "standardized.residuals")]
album2[album2$large.residual, c("cooks.distance", "leverage", "covariance.ratios")]

# Assessing Independence & Multicollinearity -------------
durbinWatsonTest(albumSales.3)

vif(albumSales.3)
1 / vif(albumSales.3)
vif(albumSales.3) %>% mean()

# Visualization of Residuals -------------
histogram <- ggplot(album2, aes(studentized.residuals)) + 
  geom_histogram(aes(y = ..density..), colour = "black", fill = "white") + 
  labs(x = "Studentized Residual", y = "Density")

histogram + stat_function(
  fun = dnorm, 
  args = list(mean = mean(album2$studentized.residuals, na.rm = TRUE), sd = sd(album2$studentized.residuals, na.rm = TRUE)),
  colour = "red", 
  linewidth = 1
)

qqplot.resid <- qplot(sample = album2$studentized.residuals, stat = "qq") + 
  labs(x = "Theoretical Values", y = "Observed Values")

album2$fitted <- albumSales.3$fitted.values

scatter <- ggplot(album2, aes(fitted, studentized.residuals))
scatter + geom_point() + geom_smooth(method = "lm", colour = "Blue") + 
  labs(x = "Fitted Values", y = "Studentized Residual")

# Bootstrapping Regressions -------------
bootReg <- function(formula, data, indices) {
  d <- data[i, ]
  fit <- lm(formula, data = d)
  return(coef(fit))
}

bootResults <- boot(
  statistic = bootReg, 
  formula = sales ~ adverts + airplay + image,
  data = album2, 
  2000
)

# Practice Test: Personality & Sex -------------
CM <- read.csv("chamorro_premuzic.csv", header = T)
CM$sex <- as.factor(CM$sex)
CM$sex <- as.numeric(CM$sex)

N <- lm(lec_neurotic ~ sex + age, data = CM, na.action = na.exclude)
summary(N)

N %>% 
  parameters::model_parameters(standardize = "refit") %>% 
  knitr::kable(digits = 3)

S_N <- rstandard(N)

O <- lm(lec_open ~ sex + age, data = CM, na.action = na.exclude)
summary(O)

# Dummy Coding: Subculture Analysis -------------
GFest <- read.csv("glastonbury.csv", header = T)
GFest.dat <- GFest[, c("day1", "day2", "day3", "subculture")]
head(GFest)

GFest$subculture <- factor(GFest$subculture)
contrasts(GFest$subculture) <- contr.treatment(4, base = 4)

hipster_v_NMA <- c(1, 0, 0, 0)
metalhead_v_NMA <- c(0, 1, 0, 0)
nosub_v_NMA <- c(0, 0, 1, 0)

contrasts(GFest$subculture) <- cbind(hipster_v_NMA, metalhead_v_NMA, nosub_v_NMA)
gfkModel <- lm(change ~ subculture, data = GFest)
summary(gfkModel)

round(tapply(GFest$change, GFest$subculture, mean, na.rm = TRUE), 3)

# Extended Practice: Salary & Possums -------------
modelCat <- read.csv("supermodel.csv", header = T)
regCat <- lm(salary ~ age + years + status, data = modelCat)
summary(regCat)

# (Diagnostics for modelCat omitted for brevity in summary but preserved in script)

possum <- read.csv("possum.csv", header = T)
possum2 <- possum[, c("hdlngth", "totlngth")]
possumModel <- lm(age ~ hdlngth + chest + belly + taill, data = possum2, na.action = na.exclude)

# Final Possum Model & Correlations -------------
correlations <- possum3 %>%
  summarise(across(everything(), ~ cor(.x, age), .names = "cor_{.col}"))

results <- sapply(possum3[, c("hdlngth", "skullw", "totlngth", "taill", "footlgth", "earconch", "eye", "chest", "belly", "age")], function(x) {
  test <- cor.test(x, possum3$age)
  c(correlation = test$estimate, p_value = test$p.value)
})

results_df <- as.data.frame(t(results))
possumFinal <- lm(age ~ belly, data = possum)
summary(possumFinal)