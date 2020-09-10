import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns

data = pd.read_csv('/home/atrides/Desktop/R/statistics_with_Python/04_Exploring_Data_with_Graphs/Data_Files/DownloadFestival.dat', sep='\s+')
print(data.head())


plt.figure
_ = plt.hist(data['day1'],bins=70)
plt.show()


_ = sns.distplot(data['day1'], color='blue')
plt.show()

