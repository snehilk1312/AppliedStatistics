# getting data into R

theDoors_names <- c('james','rey','robby','john')

theDoors_age <- c(47,50,55,49)
  

# creating DataFrames

theDoors <- data.frame(Name=theDoors_names,Age=theDoors_age)

theDoors
  
# getting ages
theDoors$Age

# getting names
theDoors$Name

# adding a column, as pandas df['childAge']=[12,12,4,6]
theDoors$childAge = c(12,12,4,6)

theDoors

# gives column names as panda s df.columns
names(theDoors)

# other way to combine variables in R ,list() and cbind()

# i) list
TheDoors <- list(theDoors_names,theDoors_age)
TheDoors
TheDoors[2]

# Calculating new variables from exisiting ones
theDoors$fatherHoodAge <- theDoors$Age - theDoors$childAge
theDoors

# clearing the variables in global enviroment
rm(list = ls())  
