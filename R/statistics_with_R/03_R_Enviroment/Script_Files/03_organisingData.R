# organising the data

# i)creating string variable
name<-c("Ben",  "Martin",  "Andy",  "Paul",  "Graham",  "Carina",  "Karina", "Doug", "Mark", "Zoe")


# ii)creating Date Variables

husband<-c("1973-06-21", "1970-07-16", "1949-10-08", "1969-05-24")
wife<-c("1984-11-12", "1973-08-02", "1948-11-11", "1983-07-23")

agegap<-husband-wife

agegap <- as.Date(husband) - as.Date(wife)
agegap
  

# iii)creating coding variables/factor

job<-c(1,1,1,1,1,2,2,2,2,2)
job

# instead we could use
jobs <- c(rep(1,5),rep(2,5))
jobs

# converting jobs into factor
jobs <- factor(jobs, levels=c(1:2), labels = c('lecturers','students'))
jobs
  
# another way
job<-gl(2, 5, labels = c("Lecturer", "Student"))
levels(job)

# iv)creating   numeric variable
friends<-c(5,2,0,4,1,10,12,15,12,17)
friends  
income<-c(20000,40000,35000,22000,50000,5000,100,3000,10000,10)
income
alcohol<-c(10,15,20,5,30,25,20,16,17,18)
alcohol
neurotic<-c(10,17,14,13,21,7,13,9,14,13)
neurotic


# creating final dataframe

lecturer_data = data.frame(name, friends, income, alcohol, neurotic,job)
lecturer_data
    

# Missing Data, use NA
neurotic<-c(10,17,NA,13,21,7,13,9,14,NA)
neurotic


# when data is missing
k = mean(neurotic,na.rm = TRUE)
k
rm(list = ls())          
