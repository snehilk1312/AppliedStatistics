import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns



data = pd.read_csv('/home/atrides/Desktop/R/statistics_with_Python/04_Exploring_Data_with_Graphs/Data_Files/DownloadFestival.dat', sep='\s+')
print(data.head())



# boxplot using seaborn
_ = sns.boxplot(x=data['gender'],y=data['day1'])
plt.show()


# Without outliers in data
data2 = pd.read_csv('/home/atrides/Desktop/R/statistics_with_Python/04_Exploring_Data_with_Graphs/Data_Files/DownloadFestival1.dat', sep='\s+')

# boxplot using pandas
_ = data2.boxplot(column='day1', by='gender')
plt.show()

# day=1
_ = sns.boxplot(x=data2['gender'],y=data2['day1'])
plt.show()

# day=2
_ = sns.boxplot(x=data2['gender'],y=data2['day2'])
plt.show()

# day=3
_ = sns.boxplot(x=data2['gender'],y=data2['day3'])
plt.show()




