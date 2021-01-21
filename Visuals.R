#Data Set Up
df<-read.csv("C:/Users/cmnot/Documents/Naissance/Naissance e-Mail Metrics Data Template.csv")

#Visual 1
library(ggplot2)
library(ggthemes)
library(plyr)
library(scales)
library(zoo)


# Plot
ggplot(Plt1Data, aes(Time, Weekday, fill = `Open Rate`)) +
  geom_tile(colour = "white") +
  facet_grid(Weekmonth~Month) +
  scale_fill_gradient(low="red", high="green") +
  labs(x="Time of Day",
       y="",
       title = "Time-Series Calendar Heatmap",
       subtitle="Email Open Rate",
       fill="Close")
#Plot 2
ggplot(Plt1Data, aes(Time, Weekday, fill = `Clickthrough Rate`)) +
  geom_tile(colour = "white") +
  facet_grid(Weekmonth~Month) +
  scale_fill_gradient(low="red", high="green") +
  labs(x="Time of Day",
       y="",
       title = "Time-Series Calendar Heatmap",
       subtitle="Email Clickthrough Rate",
       fill="Close")

#Plot 3
ggplot(Plt1Data, aes(Time, Weekday, fill = `Bounce Rate`)) +
  geom_tile(colour = "white") +
  facet_grid(Weekmonth~Month) +
  scale_fill_gradient(low="red", high="green") +
  labs(x="Time of Day",
       y="",
       title = "Time-Series Calendar Heatmap",
       subtitle="Email Bounce Rate",
       fill="Close")

#Plot 4
ggplot(Plt1Data, aes(Time, Weekday, fill = `Unsubscribe Rate`)) +
  geom_tile(colour = "white") +
  facet_grid(Weekmonth~Month) +
  scale_fill_gradient(low="red", high="green") +
  labs(x="Time of Day",
       y="",
       title = "Time-Series Calendar Heatmap",
       subtitle="Email Unsubscribe Rate",
       fill="Close")

#Plot 5
open <- c(df$Open.Rate)
click <- c(df$Clickthrough.Rate)
bounce <- c(df$Bounce.Rate)
unsub <- c(df$Unsubscribe.Rate)
aum <- c(df$Audience.Size)
data<- data.frame(open,click,bounce,unsub,aum)

library(ggplot2)
library(hrbrthemes)
library(dplyr)
library(tidyr)
library(viridis)
data<-read.csv("C:/Users/cmnot/Documents/Naissance/Plt2Data.csv")
#Interaction<-data$?..Interaction
#Rate<-data$Rate
#AUM<-data$AUM
#data<- data.frame(Interaction,Rate,AUM)
p <- ggplot(data=data, aes(x=Rate, group=Interaction, fill=Interaction)) + geom_density(adjust=1.5, alpha=.4) + theme_ipsum()
p
#Could use standardized x-axis

#Plot 6
library(ggplot2)
ggplot(data=data,aes(x=AUM,y=Rate,fill=factor(Interaction)))+
  geom_bar(stat="identity",position="dodge")+
  scale_fill_discrete(name="Interaction",
                      breaks=c(1, 2, 3, 4),
                      labels=c("Open", "Click", "Bounce", "Unsub"))+
  xlab("AUM")+ylab("Rate")
#X-axis needs to be reordered low to high; also needs a legend

#Plot 7- Haven't got it to work yet
#Follow below to download rCharts
#https://cran.r-project.org/bin/windows/Rtools/
#writeLines('PATH="${RTOOLS40_HOME}\\usr\\bin;${PATH}"', con = "~/.Renviron")
#Sys.which("make")
#install.packages("devtools")
#install.packages("Rcpp")
#(usethis)
#library(devtools)
#library(Rcpp)
#install_github('ramnathv/rCharts', force= TRUE)

library(rCharts)
library(summarytools)
n1 <- nPlot('Freq' ~ AUM,
            group = 'Interaction',
            ylab= data$Rate,
            data = data,
            type = 'multiBarChart')
n1$set(width = 600)
n1$show('iframesrc', cdn=TRUE)
n1

#Plot 8
#Compare AUM of company and averages for DVs
#Possibly with a histogram- need more data
