library(ggplot2)
library(Hmisc)

data<-read.delim('/home/atrides/Desktop/Applied-Statistics-with-R-master/statistics_with_R/04_Exploring_Data_with_Graphs/ChickFlick.dat', header=TRUE)
head(data)

# when one independent variable
barchart<-ggplot(data, aes(film,arousal))

barchart + stat_summary(fun = mean, geom = "bar", fill = "White", colour = "Black") +
  stat_summary(fun.data = mean_cl_normal, geom = "pointrange") + 
  labs(x = "Film", y = "Mean Arousal")

# errorbar instead of pointrange
barchart + stat_summary(fun = mean, geom = "bar", fill = "White", colour = "Black") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar",colour="Brown",width=0.2) + 
  labs(x = "Film", y = "Mean Arousal")


# with bootstrapped c.i
barchart + stat_summary(fun = mean, geom = "bar", fill = "White", colour = "Black") +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar",colour="Brown",width=0.2) + 
  labs(x = "Film", y = "Mean Arousal")


# several independent variable
bar<-ggplot(data, aes(film,arousal,fill=gender))
bar+stat_summary(fun=mean, geom='bar',position ='dodge')+ 
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", position = position_dodge(width=0.90), width = 0.2)


# different plot for male and female , also custom color is given using "scale_fill_manual" which applies the color on fill value passed
# while making ggplot object
bar + stat_summary(fun= mean, geom = "bar",position='dodge')+ stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = 0.2)+ facet_wrap( ~ gender)+
  labs(x = "Film", y = "Mean Arousal")+ theme(legend.position = "none")+ scale_fill_manual(values = c("red", "black"))

