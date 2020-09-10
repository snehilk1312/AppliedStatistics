import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import matplotlib.cm as cm
import seaborn as sns

data = pd.read_csv('/home/atrides/Desktop/R/statistics_with_Python/04_Exploring_Data_with_Graphs/Data_Files/Exam Anxiety.dat', sep='\s+')
data.set_index(['Code'],drop=True,inplace=True)
print(data.head())


fig, ax = plt.subplots(figsize=(10,10))
ax.set_xlabel('Anxiety')
ax.set_ylabel('Exam')
ax.set_title('Exam Performance vs Anxiety')
ax.scatter(data['Anxiety'], data['Exam'], marker = 'o')
plt.show()

corr_coeff = data['Exam'].corr(data['Anxiety'])
print(corr_coeff)

x = np.array(data['Anxiety'])
y = np.array(data['Exam'])
coef = np.polyfit(x,y, 1)
poly1d_fn = np.poly1d(coef)
_=plt.plot(x,y, 'yo',x, poly1d_fn(x), '--k')
plt.show()

# fitting line of higher orders
_ = sns.regplot(x="Anxiety", y='Exam', data=data,order=3)
plt.show()


_ = sns.lmplot(x='Anxiety', y='Exam', data=data, hue='Gender')
plt.show()



