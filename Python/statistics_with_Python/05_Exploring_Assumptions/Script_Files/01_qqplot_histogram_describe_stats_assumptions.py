import warnings
warnings.filterwarnings('ignore')

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
import scipy.stats as stats
import pingouin as pg

data = pd.read_csv('/home/atrides/Desktop/R/statistics_with_Python/05_Exploring_Assumptions/Data_Files/DownloadFestival1.dat', sep='\t')
print(data.head())

fig,ax = plt.subplots(figsize=(12, 8))
ax = plt.hist(data['day1'],density=True,bins=30, edgecolor='black', linewidth=1.4)
plt.show()

mean1 = data['day1'].mean()
sd1 = data['day1'].std()
n1 = data['day1'].count()
x = np.random.normal(mean1, sd1 ,size=n1)

day2 = data[data['day2']!=' ']
day3 = data[data['day3']!=' ']
day2['day2']=pd.to_numeric(day2['day2'])
day3['day3']=pd.to_numeric(day3['day3'])

day2['theoritical_normal'] = np.random.normal( day2['day2'].mean(), day2['day2'].std(), day2['day2'].count())
day3['theoritical_normal'] = np.random.normal( day3['day3'].mean(), day3['day3'].std(), day3['day3'].count())

# Day 1
fig,ax = plt.subplots(figsize=(12, 8))
ax = plt.hist(data['day1'],density=True,bins=30, edgecolor='black', color='white', linewidth=1.4)
ax = sns.kdeplot(data['day1'], color='black')
ax = sns.kdeplot(x, color='red')
ax = plt.xlim([0,data['day1'].max() ])
plt.show()

# Day 2
fig,ax = plt.subplots(figsize=(12, 8))
ax = plt.hist(day2['day2'],density=True,bins=30, edgecolor='black', color='white', linewidth=1.4)
ax = sns.kdeplot(day2['day2'], color='black')
ax = sns.kdeplot(day2['theoritical_normal'], color='red')
ax = plt.xlim([0,day2['day2'].max() ])
plt.show()

# Day 3
fig,ax = plt.subplots(figsize=(12, 8))
ax = plt.hist(day3['day3'],density=True,bins=30, edgecolor='black', color='white', linewidth=1.4)
ax = sns.kdeplot(day3['day3'], color='black')
ax = sns.kdeplot(day3['theoritical_normal'], color='red')
ax = plt.xlim([0,day3['day3'].max() ])
plt.show()

"""## Some QQplots"""

fig,ax = plt.subplots(figsize=(8,8))
_ = pg.qqplot(data['day1'], ax=ax, confidence=False)
plt.show()

fig,ax = plt.subplots(figsize=(8,8))
_ = pg.qqplot(day2['day2'], ax=ax, confidence=False)
plt.show()

fig,ax = plt.subplots(figsize=(10,10))
_ = pg.qqplot(day3['day3'], ax=ax, confidence=False)
plt.show()

"""## Some descriptive stats"""

print(stats.describe(data['day1']))
print(stats.describe(day2['day2']))
print(stats.describe(day3['day3']))

print(data['day1'].describe())

print(day2['day2'].describe())

print(day3['day3'].describe())

