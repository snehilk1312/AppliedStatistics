import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import matplotlib.cm as cm

data = pd.read_csv('/home/atrides/Desktop/R/statistics_with_Python/04_Exploring_Data_with_Graphs/Data_Files/FacebookNarcissism.dat', sep='\s+')
print(data.head())


print(data.info())


print(data['Rating_Type'].unique())


data['rating_type_num'] = data['Rating_Type'].replace({'Attractive':1,'Fashionable':2, 'Glamourous':3,'Cool':4})


data['Rating_jittered'] = data['Rating'] + 0.5 * np.random.rand(len(data['Rating'])) 


# plotting after adding jitter
fig, ax = plt.subplots()
fig.set_size_inches(10, 10)
scatter = ax.scatter(data['NPQC_R_Total'], data['Rating_jittered'], c=data['rating_type_num'], marker='o',cmap=cm.rainbow)
ax.set_xlabel('NPQC_R_Total')
ax.set_ylabel('Rating')
ax.set_title('Jittered Plot')
# produce a legend with the unique colors from the scatter
legend1 = ax.legend(*scatter.legend_elements(), title="Rating_Type")
legend1.get_texts()[0].set_text('Attractive')
legend1.get_texts()[1].set_text('Fashionable')
legend1.get_texts()[2].set_text('Glamourous')
legend1.get_texts()[3].set_text('Cool')
ax.add_artist(legend1)
plt.show()



# plotting without jitter
fig, ax = plt.subplots()
fig.set_size_inches(10, 10)
scatter = ax.scatter(data['NPQC_R_Total'], data['Rating'], c=data['rating_type_num'], marker='o',cmap=cm.rainbow)
ax.set_xlabel('NPQC_R_Total')
ax.set_ylabel('Rating')
ax.set_title('UnJittered Plot')
# produce a legend with the unique colors from the scatter
legend1 = ax.legend(*scatter.legend_elements(), title="Rating_Type")
legend1.get_texts()[0].set_text('Attractive')
legend1.get_texts()[1].set_text('Fashionable')
legend1.get_texts()[2].set_text('Glamourous')
legend1.get_texts()[3].set_text('Cool')
ax.add_artist(legend1)
plt.show()

groups = data.groupby('Rating_Type')


n=0
for name,group in groups:
  for x in group.values:
    if n<=5:
      print(x)
      n+=1


fig, ax = plt.subplots()
fig.set_size_inches(10, 10)
#scatter = ax.scatter(data['NPQC_R_Total'], data['Rating_jittered'],  marker='o',cmap=cm.rainbow)
ax.set_xlabel('NPQC_R_Total')
ax.set_ylabel('Rating')
ax.set_title('Jittered Plot')
count1,count2,count3,count4 = 0,0,0,0
for name,group in groups:
  for x in group.values:
    if x[2]=='Attractive':
      if count1==0:
        ax.scatter(x[1], x[5],marker='o',c='black',alpha=0.5,label='Attractive')
        count1+=1
      else:
        ax.scatter(x[1], x[5],marker='o',c='black',alpha=0.5)
    if x[2]=='Fashionable':
      if count2==0:
        ax.scatter(x[1], x[5],marker='*',c='black',alpha=0.5,label='Fashionable')
        count2+=1
      else:
        ax.scatter(x[1], x[5],marker='*',c='black',alpha=0.5)
    if x[2]=='Glamourous':
      if count3==0:
        ax.scatter(x[1], x[5],marker='v',c='black',alpha=0.5,label='Glamourous')
        count3+=1
      else:
        ax.scatter(x[1], x[5],marker='v',c='black',alpha=0.5)
    if x[2]=='Cool':
      if count4==0:
        ax.scatter(x[1], x[5],marker='x',c='black',alpha=0.5,label='Cool')
        count4+=1
      else:
        ax.scatter(x[1], x[5],marker='x',c='black',alpha=0.5)
ax.legend()
plt.show()

