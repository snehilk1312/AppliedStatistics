import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns


data = pd.read_csv('/home/atrides/Desktop/R/statistics_with_Python/04_Exploring_Data_with_Graphs/Data_Files/DownloadFestival1.dat', sep='\s+')

print(data.head())


_ = sns.kdeplot(data['day1'])
plt.show()

_ = sns.kdeplot(data['day2'])
plt.show()


_ = sns.kdeplot(data['day3'])
plt.show()



