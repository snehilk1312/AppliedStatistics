import pandas as pd
import numpy as np

lecturerData = pd.read_csv('/home/atrides/Desktop/R/statistics_with_Python/03_Python_Enviroment/Data_Files/Lecturer Data.dat', sep='\s+')


print(lecturerData)


lecturerData['job'] = lecturerData['job'].replace({1:'lecturer', 2:'students'})


# saving data
lecturerData.to_csv('final.csv')


# manipulating Data
print(lecturerData[['friends', 'income', 'neurotic']])

# lecturer only
print(lecturerData[lecturerData['job']=='lecturer'])


#  personality  variables  but  only  for  people  who  drink  more  than  10  units  of alcohol
print(lecturerData[lecturerData['alcohol']>10][["friends", "alcohol", "neurotic"]])



# Dataframe to matrix/array
print(np.array(lecturerData[lecturerData['alcohol']>10][["friends", "alcohol", "neurotic"]]))



