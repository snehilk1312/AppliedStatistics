# loading ggplot2
library(ggplot2)

facebookData <- read.delim('/home/atrides/Desktop/Applied-Statistics-with-R-master/statistics_with_R/04_Exploring_Data_with_Graphs/FacebookNarcissism.dat')
head(facebookData)


# initiatingplot objsct
graph <- ggplot(facebookData, aes(NPQC_R_Total,Rating))

# adding geoms
graph+geom_point(shape=17, size=2,aes(color=Rating_Type),position = 'jitter')

graph + geom_point(aes(shape = Rating_Type,color=Rating_Type), position = "jitter")
