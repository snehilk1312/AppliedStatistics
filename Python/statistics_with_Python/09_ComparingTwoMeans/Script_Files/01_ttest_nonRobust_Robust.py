#!/usr/bin/env python
# coding: utf-8

# In[88]:


import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
import scipy.stats as st
get_ipython().run_line_magic('matplotlib', 'inline')


# In[89]:


df_independent = pd.read_csv('/home/atrides/Desktop/R/statistics_with_Python/09_ComparingTwoMeans/Data_Files/SpiderLong.dat', sep='\t')
df_independent.head()


# ### Barplot with independent measure design
# 

# In[90]:


_ = sns.barplot(x="Group", y="Anxiety", data=df_independent)


# In[91]:


df_dependent = pd.read_csv('/home/atrides/Desktop/R/statistics_with_Python/09_ComparingTwoMeans/Data_Files/SpiderWide.dat', sep='\t')
df_dependent.head()


# In[92]:


df_dependent['mean'] = (df_dependent['picture'] + df_dependent['real'])/2


# In[93]:


grand_mean = (df_dependent['picture'].mean() + df_dependent['real'].mean())/2
print(grand_mean)


# In[94]:


df_dependent['adj'] = grand_mean - df_dependent['mean']


# In[95]:


df_dependent['picture_adj'] = df_dependent['picture'] + df_dependent['adj']
df_dependent['real_adj'] = df_dependent['real'] + df_dependent['adj']


# In[96]:


plt.figure(figsize=(10,6))
ax1 = plt.subplot(1,2,1)
_ = sns.barplot(x = ["Picture" for i in range(len(df_dependent))], y = df_dependent['picture_adj'])
ax2 = plt.subplot(1,2,2, sharey = ax1)
_ = sns.barplot(x = ['Real' for i in range(len(df_dependent))], y = df_dependent['real_adj'])


# ### we can see the errorbar is less in this barplot in comparison to the barplot generated by independent measure design .

# # Doing T-tests

# ## i) Independent t-test

# In[97]:


df_independent.head(5)


# In[98]:


df_independent.tail(5)


# In[99]:


data_picture = df_independent[df_independent['Group'] == "Picture"]
data_real    = df_independent[df_independent['Group'] != "Picture"]


# In[100]:


st.ttest_ind(data_picture['Anxiety'], data_real['Anxiety'],equal_var = False)  # welch t-test


# In[101]:


st.ttest_ind(data_picture['Anxiety'], data_real['Anxiety'],equal_var = True)


# #### don't show any significant difference

# In[102]:


_ = sns.boxplot(x="Group", y="Anxiety", data=df_independent)


# ### i) a) Robust method to compare independent means, i.e non-parametric alternatives

# In[103]:


st.ranksums(data_picture['Anxiety'], data_real['Anxiety'])


# In[104]:


st.mannwhitneyu(data_picture['Anxiety'], data_real['Anxiety'])


# ### calculating the effect size , r = sqrt(t^2/(t^2+dOf))

# In[105]:


t,p = st.ttest_ind(data_picture['Anxiety'], data_real['Anxiety'])
t


# In[106]:


dOf = (12-1) + (12-1)
dOf


# In[107]:


r = np.sqrt(t**2 / (t**2 + dOf))
r


# #### which is a medium effect size, so even the effect(i.e of differnece b/w anxiety compared in both cases were non-significant , it still represented a substantial effect.

# ## ii) Dependent/Paired t-test

# In[108]:


df_dependent.head()


# In[109]:


t, p = st.ttest_rel(df_dependent['real'], df_dependent['picture'])
print(f'statistic = {t}, pvalue = {p}')


# In[110]:


(df_dependent['real'] - df_dependent['picture']).describe()


# ### by hand

# In[111]:


se = 9.807233/np.sqrt(12)


# In[112]:


t = (df_dependent['real'] - df_dependent['picture']).mean()/se
t


# In[113]:


# confidence Interval, with help of t-table
ci_lower = (df_dependent['real'] - df_dependent['picture']).mean() - 2.179*se
ci_upper = (df_dependent['real'] - df_dependent['picture']).mean() + 2.179*se
(ci_lower, ci_upper)


# #### shows that based on this t-test there is a significant difference (because the confidence interval does not cross zero and p is less than .05) in anxiety scores across the two spider groups, it also shows how paired t-test has more power as we had learned earlier. 

# ### ii) a) Robust methods to compare dependent/paired means

# In[114]:


st.wilcoxon(df_dependent['real'], df_dependent['picture'], correction=False)


# ### Calculating the effect size

# In[115]:


dOf_paired = 12 -1
t = (df_dependent['real'] - df_dependent['picture']).mean()/se


# In[116]:


r_paired = np.sqrt(t**2 / (t**2 + dOf_paired))
r_paired


# #### effect size of this magnitude represents a very large effect (it is above .5, the threshold for a large effect). Therefore, as well as being statistically significant, this effect is large and probably substantive finding.

# In[ ]:




