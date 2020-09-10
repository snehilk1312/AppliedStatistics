import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt

data = pd.read_csv('/home/atrides/Desktop/R/statistics_with_Python/04_Exploring_Data_with_Graphs/Data_Files/Hiccups.dat', sep='\s+')
print(data.head())

print(data.describe())


data['id'] = data.index


df = pd.melt(data, id_vars='id', value_vars=['Baseline', 'Tongue', 'Carotid', 'Rectum'], var_name='Intervention', value_name='Hiccups')
df.drop(['id'], axis=1, inplace=True)
print(df.head())


_ = sns.pointplot(x='Intervention', y='Hiccups', data=df,linestyles=['--'])
plt.show()

# When there are several independent variables
da = pd.read_csv('/home/atrides/Desktop/R/statistics_with_Python/04_Exploring_Data_with_Graphs/Data_Files/TextMessages.dat', sep='\t')
print(da.head())


da_ = pd.melt(da, id_vars='Group', value_vars=['Baseline', 'Six_months'], var_name='time', value_name='num_msgs')
print(da_.tail())

_ = sns.pointplot(x='time', y='num_msgs', data=da_, hue='Group')
plt.show()

