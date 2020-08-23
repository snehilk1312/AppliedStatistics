library(ggplot2)
library(Hmisc)

data<-read.delim('/home/atrides/Desktop/Applied-Statistics-with-R-master/statistics_with_R/04_Exploring_Data_with_Graphs/Hiccups.dat', header=TRUE)

data

hiccups<-stack(data)
names(hiccups)<-c("Hiccups","Intervention")

is.factor(hiccups$Intervention)

line <- ggplot(hiccups, aes(Intervention, Hiccups))
line + stat_summary(fun = mean, geom = "point")+stat_summary(fun = mean, geom = "line", aes(group = 1),colour='Blue',linetype='dashed')+
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar",width=0.2)


# when there are several independent variables
data<-read.delim('/home/atrides/Desktop/Applied-Statistics-with-R-master/statistics_with_R/04_Exploring_Data_with_Graphs/TextMessages.dat',header=TRUE)

head(data,n=10)

library(reshape)


newData<-melt(data, id = c("Group"), measured = c("Baseline","Six_months"))
head(newData, 10)
line <- ggplot(newData, aes(variable, value, colour = Group))

line + stat_summary(fun= mean, geom = "point") + 
  stat_summary(fun= mean, geom = "line", aes(group = Group)) + 
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2) + 
  labs(x = "Time", y = "Mean Grammar Score", colour = "Group")+
  scale_y_continuous(limits = c(0, 80))
