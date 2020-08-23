library(ggplot2)

data<-read.delim('/home/atrides/Desktop/Applied-Statistics-with-R-master/statistics_with_R/04_Exploring_Data_with_Graphs/DownloadFestival.dat',header=TRUE)

# with outlier, day1
festivalBoxplot<-ggplot(data,aes(gender, day1))

festivalBoxplot+geom_boxplot()+ labs(x = "Gender", y = "Hygiene (Day 1 of Festival)")

# without outlier
data2<-read.delim('/home/atrides/Desktop/Applied-Statistics-with-R-master/statistics_with_R/04_Exploring_Data_with_Graphs/DownloadFestival1.dat',header=TRUE)

# day1
festivalBoxplotd1<-ggplot(data2,aes(gender, day1))

festivalBoxplotd1+geom_boxplot()+ labs(x = "Gender", y = "Hygiene (Day 1 of Festival)")

# day2
festivalBoxplotd2<-ggplot(data2,aes(gender, day2))

festivalBoxplotd2+geom_boxplot()+ labs(x = "Gender", y = "Hygiene (Day 2 of Festival)")

# day3
festivalBoxplotd3<-ggplot(data2,aes(gender, day3))

festivalBoxplotd3+geom_boxplot()+ labs(x = "Gender", y = "Hygiene (Day 3 of Festival)")
