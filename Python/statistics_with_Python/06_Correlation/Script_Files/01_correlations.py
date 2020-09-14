import pandas as pd
import numpy as np
import scipy.stats as st
import statsmodels.api as sm
import matplotlib.pyplot as plt
import seaborn as sns


#Calculating covariance and correlation coeffecient using hand
ad = pd.DataFrame({'watched':[5,4,4,6,8], 'bought':[8,9,10,13,15]})
print(ad)

print(ad.describe())

print(ad.corr())

_ = sns.lmplot('watched', 'bought', data=ad)
plt.show()

# cross-product deviations: multiply the deviations of one variable by the corresponding deviations of a second variable
# for above examples Cross Product Deviations
watched_diff, bought_diff = [], []
for index, row in ad.iterrows():
    watched_diff.append(5.4-row['watched'])
    bought_diff.append(11-row['bought'])


print(watched_diff, bought_diff)



Cross_Product_Deviations = sum(np.multiply(watched_diff, bought_diff))
print(Cross_Product_Deviations)


# Covariance (we got it from cross product deviations)
# covariance = Cross_Product_Deviations/(N-1)
Covariance = Cross_Product_Deviations/(5-1)
print(Covariance)

# now
watched_diff_standarized = np.array(watched_diff)/(ad['watched'].std())
bought_diff_standarized = np.array(bought_diff)/(ad['bought'].std())


Cross_Product_Deviatiions_standarized = sum(np.multiply(watched_diff_standarized, bought_diff_standarized))
print(Cross_Product_Deviatiions_standarized)


# R, correlation coefficient is standarized covariance

Covariance_standarized = Cross_Product_Deviatiions_standarized/(5-1)
print(Covariance_standarized)

# also , we can get it by R = Covariance/(s1*s2)
print(Covariance/(ad['watched'].std() * ad['bought'].std()))


# Correlations

data = pd.read_csv('/home/atrides/Desktop/R/statistics_with_Python/06_Correlation/Data_Files/Exam Anxiety.dat', sep='\t')

print(data.head())

data.set_index('Code', inplace=True, drop=True)


# pearson
print(data.corr())


# kendall
print(data.corr(method='kendall'))


# using scipy.stats, only does one pair of variables at a time

print(st.pearsonr(data['Revise'], data['Exam']))


print(st.spearmanr(data['Revise'], data['Exam']))



print(st.kendalltau(data['Revise'], data['Exam']))


print(st.pearsonr(data['Anxiety'], data['Exam']))


# Note:
# 
# use pearson r for parametric
# 
# use spearman rho for non-parametric
# 
# use tendall tau for non-parametric and small datasets

# seeing the relation with lmplot
_ = sns.lmplot('Revise', 'Exam', data=data)
plt.show()


# R^2 : measure of the amount of variability in one variable that is shared by the other


print((data.corr())**2)




