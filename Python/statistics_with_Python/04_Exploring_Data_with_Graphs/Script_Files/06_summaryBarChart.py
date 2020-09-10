import seaborn as sns
import matplotlib.pyplot as plt
import pandas as pd



data = pd.read_csv('/home/atrides/Desktop/R/statistics_with_Python/04_Exploring_Data_with_Graphs/Data_Files/ChickFlick.dat', sep='\t')
print(data.head())



_ = sns.barplot(x='film', y='arousal', data=data)
plt.show()


_ = sns.barplot(x='film', y='arousal', data=data, hue='gender')
plt.show()


