# Project: Statistical Programming Self-Study
# Source: Field, A. (2012). Discovering Statistics Using R. Chapter 5.
# Objective: Testing Parametric Assumptions.
# Date Completed: April, 2025


#Packages ------------------------------------------------------------------
install.packages("pastecs")
install.packages("car")

library(ggplot2)
library(car)
library(pastecs)
library(psych)
library(Rcmdr)



#Test ----------------------------------------------------------------
festivalData<-read.csv("download_festival.csv", header = TRUE)

festivalData<-festivalData[order(festivalData$day_1),]


#Visualising Data
Day1Hist<-ggplot(festivalData, aes(day_1))+
  geom_histogram(aes(y = ..density..), colour = "Black", fill = "White") +
  theme(legend.position = "none")+
  labs(x = "Hygiene score on day 1", y = "Density")

HistDay2<-ggplot(festivalData, aes(day_2))+
  geom_histogram(aes(y = ..density..), colour = "Black", fill = "White") +
  theme(legend.position = "none")+
  labs(x = "Hygiene score on day 2", y = "Density")

HistDay3<-ggplot(festivalData, aes(day_3))+
  geom_histogram(aes(y = ..density..), colour = "Black", fill = "White") +
  theme(legend.position = "none")+
  labs(x = "Hygiene score on day 3", y = "Density")


#Creating a normal curve
Day1Hist + stat_function(fun = dnorm, 
                         args = list(mean = mean(festivalData$day_1, na.rm = TRUE), 
                                     sd = sd(festivalData$day_1, na.rm = TRUE)),
                         colour = "Black", 
                         size = 1)


HistDay2 + stat_function(fun = dnorm,
                         args = list(mean = mean(festivalData$day_2, na.rm = TRUE), 
                                     sd = sd(festivalData$day_2, na.rm = TRUE)),
                         colour = "Black",
                         size = 1)


HistDay3 + stat_function(fun = dnorm,
                         args = list(mean = mean(festivalData$day_3, na.rm = TRUE), 
                                     sd = sd(festivalData$day_3, na.rm = TRUE)),
                         colour = "Black",
                         size = 1)

#Qplot ----------------------------------------------------------------------

qqplot_day1<-qplot(sample = festivalData$day_1)

qqplot_day2<-qplot(sample = festivalData$day_2)

qqplot_day3<-qplot(sample = festivalData$day_3)


#Quantifying Normality -----------------------------------------------------

#Obtaining major descriptive statistics
stat.desc(festivalData$day_1, basic = TRUE, norm = TRUE)


#both basic descriptive and normality tests
stat.desc(festivalData[, c("day_1", "day_2", "day_3")], basic = T, norm = T)


#rounding up the descriptive stats
round(stat.desc(festivalData[, c("day_1", "day_2", "day_3")], 
                basic = T, norm = T), 3)


#reading in a new dataset
rexam<-read.csv("r_exam.csv", header = TRUE)
rexam$uni<-factor(rexam$uni, 
                  levels = c("Duncetown University", "Sussex University"), 
                  labels = c("Duncetown University", "Sussex University"))


stat.desc(rexam[, c("exam", "computer", "lectures", "numeracy")])


#visualisation pipeline to create a density plot with a normal curve
examHist<-ggplot(rexam, aes(exam))+
  geom_histogram(aes(y = ..density..), colour = "Black", fill = "White")+
  theme(legend.position = "none")+
  labs(x = "exam scores", y = "density") + 
  stat_function(fun = dnorm, args = list(mean = mean(rexam$exam, na.rm = TRUE), 
                                         sd = sd(rexam$exam, na.rm = TRUE)), 
                colour = "Black", linewidth = 1)


computerHist<-ggplot(rexam, aes(computer))+
  geom_histogram(aes(y = ..density..), colour = "Black", fill = "White")+
  theme(legend.position = "none")+
  labs(x = "computer literacy", y = "density") + 
  stat_function(fun = dnorm, args = list(mean = mean(rexam$computer, na.rm = TRUE), 
                                         sd = sd(rexam$computer, na.rm = TRUE)), 
                colour = "Black", linewidth = 1)



lecHist<-ggplot(rexam, aes(lectures))+
  geom_histogram(aes(y = ..density..), colour = "Black", fill = "White")+
  theme(legend.position = "none")+
  labs(x = "exam scores", y = "density") + 
  stat_function(fun = dnorm, args = list(mean = mean(rexam$lectures, na.rm = TRUE), 
                                         sd = sd(rexam$lectures, na.rm = TRUE)), 
                colour = "Black", linewidth = 1)



numHist<-ggplot(rexam, aes(numeracy))+
  geom_histogram(aes(y = ..density..), colour = "Black", fill = "White")+
  theme(legend.position = "none")+
  labs(x = "Numeracy", y = "density") + 
  stat_function(fun = dnorm, args = list(mean = mean(rexam$numeracy, na.rm = TRUE), 
                                         sd = sd(rexam$numeracy, na.rm = TRUE)), 
                colour = "Black", linewidth = 1)
numHist

#simultaneous descriptive statistics for everything
desc_exam<-round(stat.desc(rexam[, c("exam", "computer", "lectures", "numeracy")], 
                           basic = TRUE, norm = TRUE), digits = 3)
desc_exam


#simple scatterplot
rGraph<-ggplot(rexam, aes(exam, lectures, colour = uni))
rGraph+geom_point()

#By -----------------------------------------------------------------------

#applying a function over some data by factor groupings
by(data = rexam$exam, INDICES = rexam$uni, FUN = describe)

by(data = rexam[, c("exam", "numeracy")], 
   INDICES = rexam$uni, FUN = stat.desc, basic = F, norm = T)


#Subsetting
sussexData<-subset(rexam, rexam$uni=="Sussex University")
dunceData<-subset(rexam, rexam$uni=="Duncetown University")


#Density Histogram by Subsetting
hist.numeracy.duncetown<-ggplot(dunceData, aes(numeracy))+
  theme(legend.position = "none")+
  geom_histogram(aes(y = ..density..), fill = "White", colour = "Black", binwidth = 1)+
  labs(x = "Numeracy Score", y = "Density")+
  stat_function(fun = dnorm, 
                args = list(mean = mean(dunceData$numeracy, na.rm = T), sd = sd(dunceData$numeracy, na.rm = T)), 
                colour = "Blue", linewidth = 1)
hist.numeracy.duncetown


hist.numeracy.sussex<-ggplot(sussexData, aes(numeracy))+
  theme(legend.position = "none")+
  geom_histogram(aes(y = ..density..), fill = "White", colour = "Black", binwidth = 1)+
  labs(x = "Numeracy Score", y = "Density")+
  stat_function(fun = dnorm, args = list(mean = mean(sussexData$numeracy, na.rm = T), sd = sd(sussexData$numeracy, na.rm = T)), colour = "Blue", linewidth = 1)
hist.numeracy.sussex


hist.exam.duncetown<-ggplot(dunceData, aes(exam))+
  theme(legend.position = "none")+
  geom_histogram(aes(y = ..density..), fill = "White", colour = "Black", binwidth = 1)+
  labs(x = "Exam Score", y = "Density")+
  stat_function(fun = dnorm, 
                args = list(mean = mean(dunceData$exam, na.rm = T), sd = sd(dunceData$exam, na.rm = T)), 
                colour = "Blue", linewidth = 1)
hist.exam.duncetown


#Testing for Normality ------------------------------------------------------
shapiro.test(rexam$exam) #shapiro_test does significance test of sample dist with norm dist
shapiro.test(rexam$numeracy)


#integrating by() with shapiro.test
by(rexam$numeracy, rexam$uni, shapiro.test)
qplot(sample = rexam$numeracy)



#Testing for Homogeneity of Variance -------------------------------------

#leveneTest does ANOVA between median variances at default
leveneTest(rexam$exam, rexam$uni, center = mean)
leveneTest(rexam$numeracy, rexam$uni)


#Transforming Data ---------------------------------------------------------
#Log transformation on scores
festivalData$logday1<-log(festivalData$day_1 + 1)
festivalData$logday2<-log(festivalData$day_2 + 1)
festivalData$logday3<-log(festivalData$day_3 + 1)


# Visualising transformed data to see changes after transformation
histLogDay1<-ggplot(festivalData, aes(logday1))+
  theme(legend.position = "none")+
  geom_histogram(aes(y = ..density..), fill = "White", colour = "Black")+
  stat_function(fun = dnorm, 
                args = list(mean = mean(festivalData$logday1, na.rm = T), 
                            sd = sd(festivalData$logday1, na.rm = T)), 
                colour = "Blue", linewidth=1)
histLogDay1


# Square Root Transformation
festivalData$sqrtDay1<-sqrt(festivalData$day_1)
festivalData$sqrtDay2<-sqrt(festivalData$day_2)
festivalData$sqrtDay3<-sqrt(festivalData$day_3)


#Inverse Transformation
festivalData$recDay1<-1/(festivalData$day_1 + 1)
festivalData$recDay2<-1/(festivalData$day_2 + 1)
festivalData$recDay3<-1/(festivalData$day_3 + 1)

#Removing outliers
festivalData$day_1noOutlier<-ifelse(festivalData$day_1 > 4, NA, festivalData$day_1)



#Test2 ----------------------------------------------------------------------

#Log transformation
rexam$logNumeracy<-log(rexam$numeracy)


#Plotting the log-transformed data
rexam.lognum.hist<-ggplot(rexam, aes(logNumeracy))

rexam.lognum.hist+geom_histogram(aes(y = ..density..), fill = "White", colour = "Black")+
  theme(legend.position = "none")+
  stat_function(fun = dnorm, 
                args = list(mean = mean(rexam$logNumeracy, na.rm = T), 
                            sd = sd(rexam$logNumeracy, na.rm = T)), 
                colour = "Blue", linewidth=1)+
  labs(x = "Numeracy Score", y = "Density")
