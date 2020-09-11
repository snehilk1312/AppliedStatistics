import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt
import numpy as np

data = pd.read_csv('/home/atrides/Desktop/R/statistics_with_Python/05_Exploring_Assumptions/Data_Files/DownloadFestival1.dat'
                  , sep='\t')
print(data.head())


day1 = pd.DataFrame(data['day1'])
day2 = pd.DataFrame(pd.to_numeric(data[data['day2']!=' ']['day2']))
day3 = pd.DataFrame(pd.to_numeric(data[data['day3']!=' ']['day3']))

print(len(day1), len(day2), len(day3))


# Log Trasformation
day1['logday1'] = np.log(day1['day1']+1)
day2['logday2'] = np.log(day2['day2']+1)
day3['logday3'] = np.log(day3['day3']+1)


# square root transformation
day1['sr1'] = np.sqrt(day1['day1'])
day2['sr2'] = np.sqrt(day2['day2'])
day3['sr3'] = np.sqrt(day3['day3'])

# reverse transformation
day1['rev'] = 1/(day1['day1']+1)
day2['rev'] = 1/(day2['day2']+1)
day3['rev'] = 1/(day3['day3']+1)


plt.figure(figsize=(15,18))

plt.subplot(431)
_ = plt.hist(day1['day1'], density=True,bins=30, edgecolor='black',color='white', linewidth=1.4)
_ = sns.kdeplot(day1['day1'], color='black')

plt.subplot(432)
_ = plt.hist(day2['day2'], density=True,bins=30, edgecolor='black',color='white', linewidth=1.4)
_ = sns.kdeplot(day2['day2'], color='black')

plt.subplot(433)
_ = plt.hist(day3['day3'], density=True,bins=30, edgecolor='black',color='white', linewidth=1.4)
_ = sns.kdeplot(day3['day3'], color='black')

plt.subplot(434)
_ = plt.hist(day1['logday1'], density=True,bins=30, edgecolor='black',color='white', linewidth=1.4)
_ = sns.kdeplot(day1['logday1'], color='black')

plt.subplot(435)
_ = plt.hist(day2['logday2'], density=True,bins=30, edgecolor='black',color='white', linewidth=1.4)
_ = sns.kdeplot(day2['logday2'], color='black')

plt.subplot(436)
_ = plt.hist(day3['logday3'], density=True,bins=30, edgecolor='black',color='white', linewidth=1.4)
_ = sns.kdeplot(day3['logday3'], color='black')

plt.subplot(437)
_ = plt.hist(day1['sr1'], density=True,bins=30, edgecolor='black',color='white', linewidth=1.4)
_ = sns.kdeplot(day1['sr1'], color='black')

plt.subplot(438)
_ = plt.hist(day2['sr2'], density=True,bins=30, edgecolor='black',color='white', linewidth=1.4)
_ = sns.kdeplot(day2['sr2'], color='black')

plt.subplot(439)
_ = plt.hist(day3['sr3'], density=True,bins=30, edgecolor='black',color='white', linewidth=1.4)
_ = sns.kdeplot(day3['sr3'], color='black')

plt.subplot(4,3,10)
_ = plt.hist(day1['rev'], density=True,bins=30, edgecolor='black',color='white', linewidth=1.4)
_ = sns.kdeplot(day1['rev'], color='black')

plt.subplot(4,3,11)
_ = plt.hist(day2['rev'], density=True,bins=30, edgecolor='black',color='white', linewidth=1.4)
_ = sns.kdeplot(day2['rev'], color='black')

plt.subplot(4,3,12)
_ = plt.hist(day3['rev'], density=True,bins=30, edgecolor='black',color='white', linewidth=1.4)
_ = sns.kdeplot(day3['rev'], color='black')

plt.show()




