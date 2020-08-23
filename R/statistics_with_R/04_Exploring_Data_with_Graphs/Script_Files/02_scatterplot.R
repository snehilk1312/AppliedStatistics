library(ggplot2)

data<-read.delim('/home/atrides/Desktop/Applied-Statistics-with-R-master/statistics_with_R/04_Exploring_Data_with_Graphs/Exam Anxiety.dat',header=TRUE)

head(data)

scatter<-ggplot(data, aes(Anxiety, Exam))

# scatterplot
scatter+geom_point()

# adding X and Y labels
scatter+geom_point()+labs(x='Exam Anxiety', y='Exam Performance %')


# adding regression line to scatterplot
scatter+geom_point()+geom_smooth(method='loess', color='red',formula='y~x',alpha=0.06,fill='Red')+labs(x='Exam Anxiety', y='Exam Performance %')

# Grouped Scatterplot
scatter <- ggplot(data, aes(Anxiety, Exam, colour = Gender))

scatter + geom_point() + geom_smooth(formula='y~x',method = "lm", aes(fill=Gender),alpha=0.1)


scatter + geom_point() + geom_smooth(formula='y~x',method = "lm", aes(fill=Gender),alpha=0.1)+ labs(x = "Exam Anxiety", y = "Exam Performance %", colour = "Gender") 
