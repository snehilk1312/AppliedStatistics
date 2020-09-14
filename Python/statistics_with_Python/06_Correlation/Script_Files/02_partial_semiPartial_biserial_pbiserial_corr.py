import pandas as pd
import numpy as np
import scipy.stats as st
import statsmodels.api as sm
import matplotlib.pyplot as plt
import seaborn as sns


# Biserial, Point biserial,  Partial Correlation and Semi-Partial Correlation

# Note:
# point-biserial correlation coefficient : discrete dichotomy
# biserial correlation : continuous dichotomy 

cat_data = pd.read_csv('/home/atrides/Desktop/R/statistics_with_Python/06_Correlation/Data_Files/pbcorr.csv')


# Point biserial

# point-biserial correlation with scipy.stats.pointbiserialr
print(st.pointbiserialr(cat_data['time'], cat_data['gender']))

# point-biserial correlation for time and gender can also be obtained by
r = cat_data['time'].corr(cat_data['gender'])
print(r)

# Now for confidence interval , as we have learnt r, doesn't have a normal sampling distribution, but Fisher has given us a way anyways

Z_r = np.arctanh(r)
print(Z_r)


N = len(cat_data)
SE_Zr = np.sqrt(1/(N-3))
lcb,ucb = Z_r-1.96*SE_Zr, Z_r+1.96*SE_Zr
(lcb, ucb) = np.tanh((lcb, ucb))
print((lcb, ucb))


# Thus coefficient of determination will be:

r_squared = r**2
print(r_squared)


# Biserial

# r_b = (r_pb * np.sqrt(p*q))/y
len_0 = len(cat_data[cat_data['gender']==0])
female_ratio = len_0/len(cat_data)
male_ratio = 1-female_ratio
q,p = (male_ratio, female_ratio)


y = 0.3977


r_biserial = r*np.sqrt(p*q)/y
print(r_biserial)


# Partial


data = pd.read_csv('/home/atrides/Desktop/R/statistics_with_Python/06_Correlation/Data_Files/Exam Anxiety.dat', sep='\t')

data = data[['Revise', 'Exam', 'Anxiety']]
print(data.head())

import pingouin as pg

print(data.pcorr())


# Using pingouin
print(pg.partial_corr(data = data, x='Exam', y='Anxiety', covar='Revise'))


# Semi-Partial Correlation

print(pg.partial_corr(data=data, x='Exam' , y='Anxiety', x_covar='Revise'))


#·partial correlation -  quantifies the relationship between two variables while controlling for the effects of a third variable on both variables in the original correlation.
# 
#·semi-partial correlation-  quantifies the relationship between two variables while controlling for the effects of a third variable on only one of the variables in the original correlation



