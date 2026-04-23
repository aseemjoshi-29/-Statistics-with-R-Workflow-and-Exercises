install.packages("mlogit")
library(car); library(mlogit)

setwd("C:/Users/Admin/Desktop/Important Documents/R/Datasets")
eelData<-read.csv("eel.csv", header = T)
head(eelData)
eelData<-eelData[, c("cured", "intervention", "duration")]
eelData$intervention<-factor(eelData$intervention, levels = c("No treatment", "Intervention"))
eelData$cured<-factor(eelData$cured, levels = c("Not cured", "Cured"))

eelData$Intervention<-relevel(eelData$Intervention, "No Treatment") #doesn't work


#Model Making####
eelModel.1 <- glm(cured ~ intervention, data = eelData, family = binomial())
eelModel.2 <- glm(cured ~ intervention + duration, data = eelData, family = binomial())

summary(eelModel.1)
summary(eelModel.2)

ModelChi<-eelModel.1$null.deviance - eelModel.1$deviance
ModelChi
chidf <- eelModel.1$df.null - eelModel.1$df.residual
chisq.prob<- 1 - pchisq(ModelChi, chidf)
chisq.prob

R2.hl<-ModelChi/eelModel.1$null.deviance
R2.hl #Hosmer-Lemeshow
R.cs <- 1 - exp ((eelModel.1$deviance - eelModel.1$null.deviance) /113)
R.cs #Cox-Snell
R.n <- R.cs /(1-(exp(-(eelModel.1$null.deviance/113))))
R.n #Nagelkerke


#Function to Calculate all 3 R2####
logisticPseudoR2s <- function(LogModel) {
  dev <- LogModel$deviance 
  nullDev <- LogModel$null.deviance 
  modelN <- length(LogModel$fitted.values)
  R.l <-  1 -  dev / nullDev
  R.cs <- 1- exp ( -(nullDev - dev) / modelN)
  R.n <- R.cs / ( 1 - ( exp (-(nullDev / modelN))))
  cat("Pseudo R^2 for logistic regression\n")
  cat("Hosmer and Lemeshow R^2  ", round(R.l, 3), "\n")
  cat("Cox and Snell R^2        ", round(R.cs, 3), "\n")
  cat("Nagelkerke R^2           ", round(R.n, 3),    "\n")
}

logisticPseudoR2s(eelModel.1)

#Odds Calculation####
exp(eelModel.1$coefficients)
exp(confint(eelModel.1)) #lower limit should be greater than 1

#Logistic Regression with Multiple Predictors####
summary(eelModel.2)

#Calculating Difference in Deviance Statistics Manually
modelChi <- eelModel.1$deviance - eelModel.2$deviance
chidf <- eelModel.1$df.residual - eelModel.2$df.residual
chisq.prob <- 1 - pchisq(modelChi, chidf)
modelChi; chidf; chisq.prob

#Calculating Difference in Deviance Statistics Automatically
anova(eelModel.1, eelModel.2)

#Save Table####
write.table(eelData, "Eel With Diagnostics.dat", sep = "/t", row.names = F)

#Diagnostics for Models####
resid(eelModel.1)
fitted(eelModel.1)
eelData$predicted.probabilities<-fitted(eelModel.1)
eelData$standardized.residuals<-rstandard(eelModel.1)
eelData$studentized.residuals<-rstudent(eelModel.1)
eelData$dfbeta<-dfbeta(eelModel.1)
eelData$dffit<-dffits(eelModel.1)
eelData $leverage<-hatvalues(eelModel.1)

head(eelData[, c("cured", "intervention", "duration", "predicted.probabilities")])

#Interpreting Residuals####
head(eelData[, c("leverage", "studentized.residuals", "dfbeta")])
#dfbeta for predictor and intercept should be below 1
#expected leverage is (k+1)/N [here, 0.18]
#only 5% resid outside 1.96, only 1% outside 2.58, cases near and above 3 warrant inspection and concern

#Multinomial Logistic Reg####
newModel<-mlogit(outcome ~ predictors, data = dataFrame, na.action = , 
                 reflevel = a number representing baseline category of the 
                 outcome)
chatModel <- mlogit(Success ~ 1 | Good_Mate + Funny + Gender + Sex + 
                      Gender:Sex +  Funny:Gender, data = mlChat, reflevel = 3)
#the : indicates interaction of those two variable classes


if(!require(remotes)){
  install.packages('remotes')
}

remotes::install_github("profandyfield/discovr")


#Test####
possum<-read.csv("possum.csv", header = T)
possum$sex<-as.factor(possum$sex)
possumL<-glm(sex ~ totlngth, data = possum, family = binomial())
summary(possumL)
possumLR<-glm(sex ~ totlngth + skullw, data = possum, family = binomial())

modelChi<-possumL$deviance - possumLR$deviance
chidf<- possumL$df.residual - possumLR$df.residual
chisq.prob<- 1 - pchisq(modelChi, chidf)
anova(possumL, possumLR)

possum$predicted.probabilities<-fitted(possumLR)
possum$standardized.residuals<-rstandard(possumLR)
possum$studentized.residuals<-rstudent(possumLR)
possum$dfbeta<-dfbeta(possumLR)
possum$dffits<-dffits(possumLR)
possum$leverage<-hatvalues(possumLR)

head(possum[, c("totlngth", "belly", "predicted.probabilities")])
round(possum[, c("leverage", "studentized.residuals", "dfbeta")], digits = 3)
