import pandas as pd
import numpy as np

# creating list variable
name = ["Ben",  "Martin",  "Andy",  "Paul",  "Graham",  "Carina",  "Karina", "Doug", "Mark", "Zoe"]
husband = ["1973-06-21", "1970-07-16", "1949-10-08", "1969-05-24"]
wife = ["1984-11-12", "1973-08-02", "1948-11-11", "1983-07-23"]


# calculating difference in age
agegap = pd.to_datetime(husband) - pd.to_datetime(wife)
print(agegap)


# creating coding variables/factors
job = [1]*5 + [2]*5
print(job)

# creating   numeric variable
friends = [5,2,0,4,1,10,12,15,12,17]
income = [20000,40000,35000,22000,50000,5000,100,3000,10000,10]
alcohol = [10,15,20,5,30,25,20,16,17,18]
neurotic = [10,17,14,13,21,7,13,9,14,13]


# creating final dataframe
lecturer_data = pd.DataFrame({'friends':friends,'income':income,'alcohol':alcohol,'neurotic':neurotic, 'job':job  })
print(lecturer_data)


lecturer_data['job'] = lecturer_data['job'].replace({1:'Lecturer', 2:'Student'})
print(lecturer_data)


# Missing Data, use None
neurotic  = [10,17,np.nan,13,21,7,13,9,14,np.nan]
print(neurotic)



# when data is missing
k = np.nanmean(neurotic)  # nanmean() function can be used to calculate the mean of array ignoring the NaN value
print(k)


