data = read.delim('/home/atrides/Desktop/Applied-Statistics-with-R-master/statistics_with_R/03_R_Enviroment/Data_Files/Honeymoon Period.dat', header=TRUE)

head(data)
names(data)
# converting wide df to long df
satisfactionStacked<-stack(data,select=c("Satisfaction_Base","Satisfaction_6_Months","Satisfaction_12_Months","Satisfaction_18_Months"))


head(satisfactionStacked)


# converting long to wide
satisfactionUnstacked <- unstack(satisfactionStacked, values~ind)
satisfactionUnstacked


# install.packages("reshape")
library(reshape)

restructuredData<-melt(data, id = c("Person", "Gender"), measured = c("Satisfaction_Base", "Satisfaction_6_Months",
                                                                      "Satisfaction_12_Months", "Satisfaction_18_Months"))

head(restructuredData)


wideData<-cast(restructuredData, Person + Gender ~ variable, value = "value")
wideData
