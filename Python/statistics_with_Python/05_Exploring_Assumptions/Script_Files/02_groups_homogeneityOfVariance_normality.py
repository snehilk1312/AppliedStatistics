import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
import scipy.stats as sp
import numpy as np
import statsmodels.api as sm

university_data = pd.read_csv('/home/atrides/Desktop/R/statistics_with_Python/05_Exploring_Assumptions/Data_Files/RExam.dat', sep='\t')
print(university_data.head())


plt.figure(figsize=(15,12))
x = np.random.normal(university_data['exam'].mean(), university_data['exam'].std(),university_data['exam'].count())
ax1 = plt.subplot(2,2,1)
_ = plt.hist(university_data['exam'],density=True,bins=30, edgecolor='black',color='white', linewidth=1.4)
_ = sns.kdeplot(university_data['exam'], color='black')
_ = sns.kdeplot(x, color='red')
_ = plt.xlim([0,university_data['exam'].max() ])
_ = ax1.set_xlabel('First yr exam score')
_ = ax1.set_ylabel('Density')

x = np.random.normal(university_data['computer'].mean(), university_data['computer'].std(),university_data['computer'].count())
ax2 = plt.subplot(2,2,2)
_ = plt.hist(university_data['computer'],density=True,bins=30, edgecolor='black',color='white', linewidth=1.4)
_ = sns.kdeplot(university_data['computer'], color='black')
_ = sns.kdeplot(x, color='red')
_ = plt.xlim([0,university_data['computer'].max() ])
_ = ax2.set_xlabel('Computer Literacy')
_ = ax2.set_ylabel('Density')

x = np.random.normal(university_data['lectures'].mean(), university_data['lectures'].std(),university_data['lectures'].count())
ax3 = plt.subplot(2,2,3)
_ = plt.hist(university_data['lectures'],density=True,bins=30, edgecolor='black', color='white',linewidth=1.4)
_ = sns.kdeplot(university_data['lectures'], color='black')
_ = sns.kdeplot(x, color='red')
_ = plt.xlim([0,university_data['lectures'].max() ])
_ = ax3.set_xlabel('Percentage of Lectures Attended')
_ = ax3.set_ylabel('Density')

x = np.random.normal(university_data['numeracy'].mean(), university_data['numeracy'].std(),university_data['numeracy'].count())
ax4 = plt.subplot(2,2,4)
_ = plt.hist(university_data['numeracy'],density=True,bins=30, edgecolor='black', color='white',linewidth=1.4)
_ = sns.kdeplot(university_data['numeracy'], color='black')
_ = sns.kdeplot(x, color='red')
_ = plt.xlim([0,university_data['numeracy'].max() ])
_ = ax4.set_xlabel('Numeracy')
_ = ax4.set_ylabel('Density')

plt.show()

# Describing basic stats
print(university_data.describe())

# Describing distributional stats
count = 0
for i in university_data.columns[:-1]:
    print('\n'*2,list(university_data.columns)[count], '\n', sp.describe(university_data[i]))
    count+=1


Dunce_data = university_data[university_data['uni']==0]
sussex_data = university_data[university_data['uni']==1]


plt.figure(figsize=(15,12))

x = np.random.normal(Dunce_data['exam'].mean(), Dunce_data['exam'].std(),Dunce_data['exam'].count())
ax1 = plt.subplot(2,2,1)
plt.hist(Dunce_data['exam'],density=True, bins=30,edgecolor='black',color='white', linewidth=1.4)
_ = sns.kdeplot(Dunce_data['exam'], color='black')
_ = sns.kdeplot(x, color='red')
_ = plt.xlim([0,Dunce_data['exam'].max() ])
ax1.set_xlabel('First yr exam score')
ax1.set_ylabel('Density')
ax1.set_title('Duncetown University')

x = np.random.normal(sussex_data['exam'].mean(), sussex_data['exam'].std(),sussex_data['exam'].count())
ax2 = plt.subplot(2,2,2,sharey=ax1,sharex=ax1)
plt.hist(sussex_data['exam'],density=True, bins=30,edgecolor='black',color='white', linewidth=1.4)
_ = sns.kdeplot(sussex_data['exam'], color='black')
_ = sns.kdeplot(x, color='red')
_ = plt.xlim([0,sussex_data['exam'].max() ])
ax2.set_xlabel('First yr exam score')
ax2.set_ylabel('Density')
ax2.set_title('Sussex University')

x = np.random.normal(Dunce_data['numeracy'].mean(), Dunce_data['numeracy'].std(),Dunce_data['numeracy'].count())
ax3 = plt.subplot(2,2,3)
plt.hist(Dunce_data['numeracy'],density=True, bins=15,edgecolor='black',color='white', linewidth=1.4)
_ = sns.kdeplot(Dunce_data['numeracy'], color='black')
_ = sns.kdeplot(x, color='red')
_ = plt.xlim([0,Dunce_data['numeracy'].max() ])
ax3.set_xlabel('Numeracy')
ax3.set_ylabel('Density')

x = np.random.normal(sussex_data['numeracy'].mean(), sussex_data['numeracy'].std(),sussex_data['numeracy'].count())
ax4 = plt.subplot(2,2,4,sharex=ax3, sharey=ax3)
plt.hist(sussex_data['numeracy'],density=True, bins=15,edgecolor='black',color='white', linewidth=1.4)
_ = sns.kdeplot(sussex_data['numeracy'], color='black')
_ = sns.kdeplot(x, color='red')
_ = plt.xlim([0,sussex_data['numeracy'].max() ])
ax4.set_xlabel('Numeracy')
ax4.set_ylabel('Density')
               
plt.show()


plt.figure(figsize=(15,12))

x = np.random.normal(Dunce_data['computer'].mean(), Dunce_data['computer'].std(),Dunce_data['computer'].count())
ax1 = plt.subplot(2,2,1)
plt.hist(Dunce_data['computer'],density=True, bins=30,edgecolor='black',color='white', linewidth=1.4)
_ = sns.kdeplot(Dunce_data['computer'], color='black')
_ = sns.kdeplot(x, color='red')
_ = plt.xlim([0,Dunce_data['computer'].max() ])
ax1.set_xlabel('computer')
ax1.set_ylabel('Density')
ax1.set_title('Duncetown University')

x = np.random.normal(sussex_data['computer'].mean(), sussex_data['computer'].std(),sussex_data['computer'].count())
ax2 = plt.subplot(2,2,2,sharey=ax1,sharex=ax1)
plt.hist(sussex_data['computer'],density=True, bins=30,edgecolor='black',color='white', linewidth=1.4)
_ = sns.kdeplot(sussex_data['computer'], color='black')
_ = sns.kdeplot(x, color='red')
_ = plt.xlim([0,sussex_data['computer'].max() ])
ax2.set_xlabel('computer')
ax2.set_ylabel('Density')
ax2.set_title('Sussex University')

x = np.random.normal(Dunce_data['lectures'].mean(), Dunce_data['lectures'].std(),Dunce_data['lectures'].count())
ax3 = plt.subplot(2,2,3)
plt.hist(Dunce_data['lectures'],density=True, bins=15,edgecolor='black',color='white', linewidth=1.4)
_ = sns.kdeplot(Dunce_data['lectures'], color='black')
_ = sns.kdeplot(x, color='red')
_ = plt.xlim([0,Dunce_data['lectures'].max() ])
ax3.set_xlabel('lectures')
ax3.set_ylabel('Density')

x = np.random.normal(sussex_data['lectures'].mean(), sussex_data['lectures'].std(),sussex_data['lectures'].count())
ax4 = plt.subplot(2,2,4,sharex=ax3, sharey=ax3)
plt.hist(sussex_data['lectures'],density=True, bins=15,edgecolor='black',color='white', linewidth=1.4)
_ = sns.kdeplot(sussex_data['lectures'], color='black')
_ = sns.kdeplot(x, color='red')
_ = plt.xlim([0,sussex_data['lectures'].max() ])
ax4.set_xlabel('lectures')
ax4.set_ylabel('Density')
               
plt.show()


# Doing Statistical tests for normality assumptions
# shapiro-wilk test on whole data
print(sp.shapiro(university_data['exam']))      # violates normality
print(sp.shapiro(university_data['computer']))  # normal
print(sp.shapiro(university_data['lectures']))  # normal
print(sp.shapiro(university_data['numeracy']))  # violated normality

# shapiro-wilk test on Duncetown data
print(sp.shapiro(Dunce_data['exam']))      # normal
print(sp.shapiro(Dunce_data['computer']))  # normal
print(sp.shapiro(Dunce_data['lectures']))  # normal
print(sp.shapiro(Dunce_data['numeracy']))  # violated normality

# shapiro-wilk test on Sussex data
print(sp.shapiro(sussex_data['exam']))      # normal
print(sp.shapiro(sussex_data['computer']))  # violated normality
print(sp.shapiro(sussex_data['lectures']))  # normal
print(sp.shapiro(sussex_data['numeracy']))  # violated normality

_ = sm.qqplot(university_data['exam'])  # see deviation from the 45 degree line
plt.show()


_ = sm.qqplot(university_data['numeracy'])   # see deviation from the 45 degree line
plt.show()


# Doing Statistical tests for homogeneity of variance
# levene test for exam score b/w Dunce and sussex¶
print(sp.levene(Dunce_data['exam'], sussex_data['exam']))   # non-significant

# levene test for numeracy b/w Dunce and sussex¶
print(sp.levene(Dunce_data['numeracy'], sussex_data['numeracy']))   # significant deviation

