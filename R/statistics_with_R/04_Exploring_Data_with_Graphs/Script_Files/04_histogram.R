library(ggplot2)

data<-read.delim('/home/atrides/Desktop/Applied-Statistics-with-R-master/statistics_with_R/04_Exploring_Data_with_Graphs/DownloadFestival.dat', header=TRUE)

head(data)

festivalHistogram <- ggplot(data, aes(day1)) + theme(legend.position = "none")

festivalHistogram + geom_histogram(binwidth = 0.4)+ labs(x = "Hygiene (Day 1 of Festival)", y = "Frequency")
