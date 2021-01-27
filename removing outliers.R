Complete_data2<-read.csv("~/R/Complete_data.csv")

library(readxl)
library(tidyverse)
library(stringr)
library(psych)
library(caret)
library(ggplot2)
library(car)
library(ggthemes)
library(plyr)
library(scales)
library(zoo)
library(descr)
library(dplyr)
library(tidymodels)
library(class)
library(kknn)
library(janitor)
library(CGPfunctions)
library(summarytools)
library(ggpubr)
library(stringi)
library(hrbrthemes)
library(tidyr)
library(viridis)
library(e1071)
library(moments)
library(rstatix)
library(rpart)
library(rpart.plot)
library(randomForest)
library(caTools)
library(arsenal)
library(qwraps2)
library(gridExtra)
library(grid)
#Outliers
Out.Audience.Size<-boxplot.stats(Complete_data2$Audience.Size)$out
Out.Open.Rate<-boxplot.stats(Complete_data2$Opened)$out
Out.Clickthrough.Rate<-boxplot.stats(Complete_data2$Clicked)$out
Out.Bounce.Rate<-boxplot.stats(Complete_data2$Bounced)$out
Out.Unsubscibe.Rate<-boxplot.stats(Complete_data2$Unsubscribed)$out

#Boxplot
par(mfrow=c(2,3))
g1<-ggplot(Complete_data2) +
  aes(x="",y=Audience.Size)+
  geom_boxplot(fill="#0c4c8a")+
  theme_minimal()

g2<-ggplot(Complete_data2) +
  aes(x = "", y = Opened) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()

g3<-ggplot(Complete_data2) +
  aes(x = "", y = Clicked) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()

g4<-ggplot(Complete_data2) +
  aes(x = "", y =Bounced) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()

g5<-ggplot(Complete_data2) +
  aes(x = "", y = Unsubscribed) +
  geom_boxplot(fill = "#0c4c8a") +
  theme_minimal()

require(gridExtra)
grid.arrange(g1, g2, g3, g4, g5, ncol=5,top=textGrob(
  "Before removing outliers and NAs",
  gp = gpar(fontsize = 16)))

##Eliminating outliers

Q <- quantile(data$Audience.Size, probs=c(.25, .75), na.rm = FALSE)
iqr <- IQR(data$Audience.Size)
up <-  Q[2]+1.5*iqr # Upper Range  
low<- Q[1]-1.5*iqr # Lower Range
eliminated<- subset(data, data$Audience.Size > (Q[1] - 1.5*iqr) & data$Audience.Size < (Q[2]+1.5*iqr))


Q <- quantile(eliminated$Opened, probs=c(.25, .75), na.rm = FALSE)
iqr <- IQR(eliminated$Opened)
up <-  Q[2]+1.5*iqr # Upper Range  
low<- Q[1]-1.5*iqr # Lower Range
eliminated<- subset(eliminated, eliminated$Opened > (Q[1] - 1.5*iqr) & eliminated$Opened < (Q[2]+1.5*iqr))

Q <- quantile(eliminated$Clicked, probs=c(.25, .75), na.rm = FALSE)
iqr <- IQR(eliminated$Clicked)
up <-  Q[2]+1.5*iqr # Upper Range  
low<- Q[1]-1.5*iqr # Lower Range
eliminated<- subset(eliminated, eliminated$Clicked > (Q[1] - 1.5*iqr) & eliminated$Clicked < (Q[2]+1.5*iqr))

Q <- quantile(eliminated$Bounced, probs=c(.25, .75), na.rm = TRUE)
iqr <- IQR(eliminated$Bounced, na.rm=TRUE)
up <-  Q[2]+1.5*iqr # Upper Range  
low<- Q[1]-1.5*iqr # Lower Range
eliminated<- subset(eliminated, eliminated$Bounced > (Q[1] - 1.5*iqr) & eliminated$Bounced < (Q[2]+1.5*iqr))

eliminated<- subset(eliminated, eliminated$Unsubscribed < 0.01)

Complete_data2<-eliminated

write.csv(Complete_data2, "Complete_data3.csv") 

######### Normality assumptions
data<-Complete_data2
par(mfrow=c(2,2))
set.seed(1234)
shapiro.test(data$Opened)
zscores<-scale(data$Opened)
qqnorm(zscores, main="Opened QQ plot")
qqline(zscores)

set.seed(49)
shapiro.test(data$Clicked)
zscores<-scale(data$Clicked)
qqnorm(zscores, main="Clicked QQ plot")
qqline(zscores)

set.seed(491)
shapiro.test(data$Audience.Size)
zscores<-scale(data$Audience.Size)
qqnorm(zscores, main="Audience size QQ plot")
qqline(zscores)

set.seed(498)
shapiro.test(data$Bounced)
zscores<-scale(data$Bounced)
qqnorm(zscores, main="Bounced QQ plot")
qqline(zscores)

set.seed(493)
shapiro.test(data$Unsubscribed)
zscores<-scale(data$Unsubscribed)
qqnorm(zscores)
qqline(zscores)


