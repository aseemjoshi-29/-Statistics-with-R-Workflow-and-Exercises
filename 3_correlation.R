# Project: Statistical Programming Self-Study
# Source: Field, A. (2012). Discovering Statistics Using R. Chapter 6.
# Objective: Data Correlation.
# Date Completed: April, 2025





# Packages --------------------------------------------------------------
install.packages("ggm")
install.packages("polycor")
install.packages("correlation")

library(ggplot2)
library(ggm)
library(polycor)
library(Hmisc)
library(boot)
library(correlation)
library(dplyr)


# Initial Data Prep -------------------------------------------------------
liarData$novice <- as.factor(liarData$novice)

adverts <- c(5, 4, 4, 6, 8)
packets <- c(8, 9, 10, 13, 15)
advertData <- data.frame(adverts, packets)

advertPlot <- ggplot(advertData, aes(adverts, packets))

advertPlot + geom_point() + geom_smooth(method = "lm", se = F)

# Pearson Correlation ------------------------------------------------
examData <- read.csv("exam_anxiety.csv", header = T)

cor(examData$exam_grade, examData$anxiety, use = "pairwise.complete.obs", method = "pearson")
cor.test(examData$exam_grade, examData$anxiety, method = "pearson", conf.level = 0.99)

head(examData)

cor(examData[, c("exam_grade", "anxiety", "revise")])
examData2 <- examData[, c("exam_grade", "anxiety", "revise")]

examMatrix <- as.matrix(examData[, c("exam_grade", "anxiety", "revise")])
examMatrix

Hmisc::rcorr(examMatrix, type = "pearson")

cor.test(examData$revise, examData$exam_grade, method = "pearson", conf.level = 0.95)
cor.test(examData$revise, examData$anxiety, method = "pearson", conf.level = 0.95)

cor(examData[, c("exam_grade", "anxiety", "revise")])^2 * 100


# Spearman Correlation ------------------------------------------------
liarData <- read.csv("biggest_liar.csv", header = T)

cor(liarData$position, liarData$creativity, use = "pairwise.complete.obs", method = "spearman")
cor.test(liarData$position, liarData$creativity, alternative = "less", method = "kendall")

cor.test(adverts, packets, method = "pearson", conf.level = 0.95)


# Bootstrapping -------------------------------------------------------
bootTau <- function(liarData, i) {
  cor(liarData$position[i], liarData$creativity[i], use = "complete.obs", method = "kendall")
}

boot_kendall <- boot::boot(liarData, bootTau, 2000)
boot_kendall
boot::boot.ci(boot_kendall)

examBootRHO <- function(examData2, i) {
  cor(examData2$exam_grade[i], examData2$revise[i], use = "complete.obs", method = "spearman")
}

examBootSpearman <- boot::boot(examData2, examBootRHO, 2000)
examBootSpearman
boot::boot.ci(examBootSpearman)

# Biserial Correlation ----------------------------------------------
catData <- read.csv("roaming_cats.csv", header = T)
catData <- catData[, c("time", "sex")]
cor.test(catData$time, catData$sex, method = "pearson")

catFrequencies <- table(catData$sex)
prop.table(catFrequencies)
polycor::polyserial(catData$time, catData$sex)

corrtable::correlation_matrix(
  examData2, 
  type = "pearson", 
  digits = 3,
  use = "complete.obs", 
  show_significance = T,
  decimal.mark = "."
)

# Partial Correlations --------------------------------------------------
pc <- ggm::pcor(c("exam_grade", "anxiety", "revise"), var(examData2))
pc^2

pcortest <- pcor.test(pc, 1, 103)
pcortest$tval
pcortest$df
pcortest$pvalue

examDatMen <- subset(examData, sex == "Male", c("revise", "exam_grade", "anxiety"))
examDatWomen <- subset(examData, sex == "Female", c("revise", "exam_grade", "anxiety"))

cor.test(examDatMen$exam_grade, examDatMen$anxiety, method = "pearson")
cor.test(examDatWomen$exam_grade, examDatWomen$anxiety, method = "pearson")


# Final Tests and Factor Mapping -------------------------------------------
essayData <- read.csv("essay_marks.csv", header = T)
essayData <- essayData[, c("essay", "hours", "grade")]

cor.test(essayData$hours, essayData$essay, method = "kendall")
cor.test(essayData$hours, essayData$grade, method = "kendall")

essayData$grade <- factor(
  essayData$grade, 
  levels = c("First class", "Upper second class", "Lower second class", "Third class"), 
  labels = c("1", "2", "3", "4")
)

essayData$gradeNum <- as.numeric(essayData$grade)
cor.test(essayData$hours, essayData$gradeNum, method = "spearman")

essayPlot <- ggplot(essayData, aes(hours, essay, colour = grade))
essayPlot + geom_point(show.legend = T)

gradesData <- read.csv("grades.csv", header = T)

gradesData$stats <- as.factor(gradesData$stats)
gradesData$stats <- factor(
  gradesData$stats, 
  levels = c("First class", "Upper second class", "Lower second class", "Third class", "Pass", "Fail"), 
  labels = c("1", "2", "3", "4", "5", "6")
)
gradesData$statsNum <- as.numeric(gradesData$stats)                  

gradesData$gcse <- as.factor(gradesData$gcse)
gradesData$gcse <- factor(
  gradesData$gcse, 
  levels = c("A", "B", "C", "D", "E", "F"), 
  labels = c("1", "2", "3", "4", "5", "6")
)
gradesData$gcseNum <- as.numeric(gradesData$gcse)

cor.test(gradesData$statsNum, gradesData$gcseNum, method = "spearman")
correlation::correlation(gradesData, select = c("statsNum", "gcseNum"), method = "kendall")

liarData$novice_num <- as.numeric(liarData$novice)