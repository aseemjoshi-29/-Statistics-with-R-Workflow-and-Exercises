# Project: Statistical Programming Self-Study
# Source: Field, A. (2012). Discovering Statistics Using R. Chapter 19.
# Objective: Multilevel Linear Models
# Date Completed: June, 2025


# Packages --------------------------------------------------------

install.packages("nlme")

library(car)
library(ggplot2)
library(nlme)
library(reshape)


# Data Creation and Importing -------------------------------------------

surgeryData = read.delim("Cosmetic Surgery.dat",  header = T)


# Mock ANOVA and ANCOVA --------------------------------------------------

# ANOVA
surgeryLinearModel <- lm(Post_QoL ~ Surgery, data = surgeryData)
summary(surgeryLinearModel)


# ANCOVA
surgeryLinearModel <- lm(Post_QoL ~ Surgery + Base_QoL, data = surgeryData)
summary(surgeryLinearModel)

# Checking need for MLM, Step 1 : Check baseline intercept model
interceptOnly <- gls(Post_QoL ~ 1, data = surgeryData, method = "ML")
summary(interceptOnly)

# Step 2 : Next check a intercept model which varies slopes across the grouping var
randomInterceptOnly <-lme(Post_QoL ~ 1, data = surgeryData, 
                          random = ~1|Clinic, method = "ML")
summary(randomInterceptOnly)


# Step 3: Derive log likelihood for each model, this is x2

logLik(interceptOnly) * -2
logLik(randomInterceptOnly) * -2 

# (Random intercepts x2 - intercepts x2, random intercepts df - intercepts df)
# check for significance in chi square table


# also compare with Anova()
Anova(interceptOnly, randomInterceptOnly) #misspelled  in the book



# Analysis  ---------------------------------------------------------


# Adding fixed effects
randomInterceptSurgery <- lme(Post_QoL ~ Surgery, data = surgeryData, 
                             random = ~1|Clinic, method = "ML")

randomInterceptSurgeryQoL <- lme(Post_QoL ~ Surgery + Base_QoL, 
                                data = surgeryData, 
                                random = ~1|Clinic, 
                                method = "ML")

Anova(randomInterceptOnly, randomInterceptSurgery, randomInterceptSurgeryQoL)


# Introducing random slopes
addRandomSlope <- lme(Post_QoL ~ Surgery + Base_QoL, data = surgeryData, 
                      random = ~Surgery|Clinic, method = "ML")

Anova(randomInterceptSurgeryQoL,addRandomSlope)


# Adding Interaction Term to the Model
addReason <- lme(Post_QoL ~ Surgery + Base_QoL + Reason, data = surgeryData, 
               random = ~Surgery|Clinic, method = "ML")



# Final Model
finalModel <- lme(Post_QoL ~ Surgery + Base_QoL + Reason + Surgery:Reason, 
                  data = surgeryData, 
                  random = ~Surgery|Clinic, 
                  method = "ML")


Anova(addRandomSlope, addReason, finalModel)


# Subsetting the model according to reasons for surgery
physicalSubset <- surgeryData$Reason==1
cosmeticSubset<- surgeryData$Reason==0


# Comparing both models
physicalModel<-lme(Post_QoL ~ Surgery + Base_QoL, 
                   data = surgeryData, 
                   random = ~Surgery|Clinic, 
                   subset= physicalSubset, 
                   method = "ML")

cosmeticModel<-lme(Post_QoL ~ Surgery + Base_QoL, 
                   data = surgeryData, 
                   random = ~Surgery|Clinic, 
                   subset= cosmeticSubset, 
                   method = "ML")


# Qol post-surgery lower for cosmetic subset than physical subset




# Growth Models -------------------------------------------------------

# Data Import
satisfactionData = read.delim("Honeymoon Period.dat",  header = T)


# Baseline Model
intercept <-gls(Life_Satisfaction ~ 1, data = restructuredData, 
                method = "ML", na.action = na.exclude)

# Random Intercepts
randomIntercept <-lme(Life_Satisfaction ~ 1, 
                      data = restructuredData, 
                      random = ~1|Person, 
                      method = "ML",  
                      na.action = na.exclude, 
                      control = list(opt="optim"))


# Adding time as a fixed effect 
timeRI<-update(randomIntercept, .~. + Time)

# random slopes
timeRS<-update(timeRI, random = ~Time|Person)

# autocorrelated model
ARModel<-update(timeRS, correlation = corAR1(0, form = ~Time|Person))


#comparing models
Anova(intercept, randomIntercept, timeRI, timeRS, ARModel)



# Adding higher order polynomials
# Quadratic
timeQuadratic<-update(ARModel, .~. + I(Time^2))

# Cubic
timeCubic <-update(timeQuadratic, .~. + I(Time^3))

# Comparison
anova(ARModel, timeQuadratic, timeCubic)

summary(timeCubic)
intervals(timeCubic)