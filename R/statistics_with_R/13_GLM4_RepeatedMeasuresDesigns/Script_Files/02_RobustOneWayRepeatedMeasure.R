library(ez)
library(nlme)
library(ggplot2)
library(multcomp)
library(pastecs)
library(WRS)
library(reshape)


df<- read.delim('/home/atrides/Desktop/R/statistics_with_R/13_GLM4_RepeatedMeasuresDesigns/Data_Files/Bushtucker.dat', header = TRUE)

# we will use the package WRS, which needs the data in wide format 

# we will use functions as rmanova(), rmmcp() , rmanovab(), pairdepb()

df<- df[, -c(1)]

# one way repeated measure anova based on trimmed means, rmanova(data, tr = .2)
rmanova(df)

# performs one-way repeated-measures ANOVA using a bootstrap procedure, rmanovab(data, tr = .2, alpha = .05, nboot = 599) 
rmanovab(df, nboot=2000)

# performs post hoc tests for one-way repeated-measures design based on trimmed means.
rmmcp(df)


# performs post hoc test according to rmanovab()
pairdepb(df, nboot = 2000)


# Conclusion: According to all robust anova and post-hoc tests , none of groups differ significantly

