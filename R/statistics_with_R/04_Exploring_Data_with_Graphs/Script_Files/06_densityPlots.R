library(ggplot2)

data<-read.delim('/home/atrides/Desktop/Applied-Statistics-with-R-master/statistics_with_R/04_Exploring_Data_with_Graphs/DownloadFestival1.dat',header=TRUE)

head(data)

day1 <- ggplot(data, aes(day1))

day1+geom_density(color='red') + labs(x='hygiene day1 ', y='density estimate')



day2 <- ggplot(data, aes(day2))

day2+geom_density(color='black') + labs(x='hygiene day2 ', y='density estimate')


day3 <- ggplot(data, aes(day3))

day3+geom_density(color='green') + labs(x='hygiene day3 ', y='density estimate')

