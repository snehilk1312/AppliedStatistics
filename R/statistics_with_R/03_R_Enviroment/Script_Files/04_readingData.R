# reading from a dat file
lecturerData <- read.delim('/home/atrides/Desktop/Applied-Statistics-with-R-master/statistics_with_R/03_learningR/Lecturer Data.dat', header=TRUE)
lecturerData

?gl
job<- gl(2,5,length=2*5,labels=c('lecturers','students'))
job


# refactoring

job <- factor(job, levels=levels(job)[c(2,1)])
job

lecturerData

# saving Data
write.csv(lecturerData, 'df.csv')

# manipulating data
newData <- lecturerData[, c('friends','income','neurotic')]
newData

# lecturer only
lecturer <- lecturerData[job=='lecturers',]
lecturer

#  personality  variables  but  only  for  people  who  drink  more  than  10  units  of alcohol
alcoholPersonality <- lecturerData[lecturerData$alcohol>10, c("friends", "alcohol", "neurotic")]
alcoholPersonality

# Dataframe to matrix
alcoholPersonalityMatrix <- as.matrix(alcoholPersonality)
alcoholPersonalityMatrix
