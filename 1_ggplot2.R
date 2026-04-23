# Project: Statistical Programming Self-Study
# Source: Field, A. (2012). Discovering Statistics Using R. Chapter 4.
# Objective: Visualization with ggplot2.
# Date Completed: April, 2025


#Packages -----------------------------------------------------------------

library(ggplot2)
library(reshape)


#Facet Positioning for Graphs -----------------------------------------------

facet_grid() #facet_grid(x ~ y)
facet_wrap() #facet wrap( ~ y, nrow = integer, ncol = integer)

#Saving Graphs ---------------------------------------------------------------
ggsave()


#Exercises ------------------------------------------------------------------
setwd("C:/Users/Admin/Desktop/Important Documents/R/Datasets")
facebookData<-read.csv("ong_2011.csv", header = TRUE)


molten1 <- melt(facebookData, 
              id=c("age", "sex", "status", "narcissism", "extraversion"), 
              measured=c("attractive", "fashion", "glamour", "cool"))

molten2 <- melt.data.frame(facebookData, 
                           id.vars = c("age", "sex", "status", "narcissism", "extraversion"), 
                           measure.vars = c("attractive", "fashion", "glamour", "cool"), variable_name = "NPQC_Values")


graph<-ggplot(molten2, aes(narcissism, value))

graph+geom_point(aes(colour = NPQC_Values))


#Scatterplots -------------------------------------------------------------
examData<-read.csv("exam_anxiety.csv", header = TRUE)

#Trying out scatterplot creation
scatter<-ggplot(examData, aes(anxiety, exam_grade))
scatter+geom_point()+labs(x = "Exam Anxiety", y = "Exam Performance in %")


scatter<-ggplot(examData, aes(anxiety, exam_grade, colour = sex))

scatter+
  geom_point()+
  geom_smooth(method = "lm", alpha=0.1)+
  labs(x = "Exam Anxiety", y = "Exam Performance %", colour = "Gender")
  

#Regression Lines ------------------------------------------------------------
scatter + 
  geom_point() +
  geom_smooth(method = "lm", colour = "Red", alpha = 0.1, fill = "Green") +
  labs(x="Exam Anxiety", y="Exam Performance %")



#Test1 ----------------------------------------------------------------------
facebookGraph<-ggplot(molten2, aes(narcissism, value, colour = NPQC_Values))

facebookGraph+geom_smooth(method = "lm", se=F)+
  geom_point(position = "jitter")+
  labs(x = "Narcissism Scores", y = "Scale Values")



#Histograms -----------------------------------------------------------------
festivalData<-read.csv("download_festival.csv", header = TRUE)

festivalHistogram<-ggplot(festivalData, aes(day_1))

#frequency histogram to check normal curve 
festivalHistogram + geom_histogram(binwidth = 0.4)+
  labs(x = "Hygiene(Festival Day 1)", y = "Frequency")



#Box Plots ------------------------------------------------------------------
festivalBoxplot<-ggplot(festivalData, aes(gender, day_1))
festivalBoxplot+geom_boxplot()+
  labs(x = "Gender", y = "Hygiene Day 1 of Festival")

festivalData<-festivalData[order(festivalData$day_1),] #reordering the scores



#Density Plots ----------------------------------------------------------------
density<-ggplot(festivalData, aes(day_1))

density + 
  geom_density() + 
  labs(x = "Hygiene (Day 1 of Festival)", y = "Density Estimate")


#Bar Graph ----------------------------------------------------------------
#You've gotta do stats summary along with it
bar<-ggplot(dataset, aes(x, y))

stat_summary(function = x, geom = y) #general form

bar + stat_summary(fun.y = mean, geom = "bar", fill = "White", colour = "Black")

stat_summary(fun.data = mean_cl_normal, geom = "pointrange")


#Several Independent Variables
bar<-ggplot(chickFlick, aes(film, arousal, fill = gender))

bar+stat_summary(fun.y = mean, geom="bar", position="dodge")+
  stat_summary(fun.data = mean_cl_normal, geom="errorbar", position = position_dodge(width=0.90), width=0.2)+
  labs(x = "Film", y = "Mean Arousal", fill = "Gender")


#Describing Gender as a Facet
bar <- ggplot(chickFlick, aes(film, arousal, fill = film))

bar + stat_summary(fun.y = mean, geom="bar")+
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2)+
  facet_wrap( ~ gender)+
  labs(x = "Film", y = "Mean Arousal")+
  scale_fill_manual("Gender", c("Female"="#3366FF", "Male"="#336633"))


#Line Graphs ---------------------------------------------------------------

#Reading in the data
hiccupsData<-read.csv("hiccups.csv", header = TRUE)

line<-ggplot(hiccupsData, aes(intervention, hiccups))

line+stat_summary(fun.y = mean, geom = "line", aes(group=1), colour="Blue", linetype="dashed")+
  stat_summary(fun.data=mean_cl_normal, geom = "errorbar", width=0.2)+
  labs(x = "Intervention", y = "Mean Number of Hiccups")

names(hiccups)<-c("hiccups", "intervention")


#Several Independent Variables
lines<-ggplot(dataFrame, aes(data1, data2, colour = Group))

line + stat_summary(fun.y = mean, geom = "point")+
  stat_summary(fun.y = mean, geom = "line", aes(group = Group))+
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2)+
  labs(x = "Time", y = "Mean Grammar Score", colour = "Group")


#Themes ----------------------------------------------------------------------

graph+geom_point(aes(colour = NPQC_Values)) + theme()


#Test2 -----------------------------------------------------------------------

#Create bar + errorbar with pointers on the data
lecturerData<-read.csv("lecturerData.csv", header = TRUE)

lecturergraph<-ggplot(lecturerData, aes(job, neurotic, fill = job))

lecturergraph+stat_summary(fun.y = mean, geom = "bar")+
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width=0.2)+
  +   stat_summary(fun.y = mean, geom = "point")


#melt data frame and create a line graph with error bars and points at score means
lecLineData2 <- melt.data.frame(lecturerData, 
                              id.vars = "neurotic", 
                              measure.vars = "job", 
                              variable_name = "Job")

lecLinePlot2<-ggplot(lecLineData2, aes(value, neurotic))

lecLinePlot2 + stat_summary(fun.y = mean, geom = "line", group=1, linetype = "dashed")+
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width=0.2) +
  stat_summary(fun.y = mean, geom="point", colour="Red")


#Scatterplot with a regression line
lecturerScatter<-ggplot(lecturerData, aes(neurotic, alcohol, colour = job))

lecturerScatter+geom_point()+geom_smooth(method = "lm", alpha=0.2)


#Bargraph + Errorbars experimenting with the width, position and fill
Infidelity<-read.csv("Infidelity.csv", header = TRUE)

moltenInfidelity<-melt.data.frame(Infidelity, 
                                  id.vars = "X", 
                                  measure.vars = c("malePartnersFace", 
                                                   "maleOwnFace", 
                                                   "femalePartnersFace",
                                                   "femaleOwnFace"))

infidelityLine<-ggplot(moltenInfidelity, aes(variable, value, fill = variable))

infidelityLine+stat_summary(fun.y = mean, geom = "bar")+
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", 
               position = position_dodge(width=0.90), width=0.2)