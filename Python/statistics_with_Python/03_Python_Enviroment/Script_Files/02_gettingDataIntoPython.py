#!/usr/bin/env python
# coding: utf-8

import pandas as pd

theDoors_names = ['james','rey','robby','john']
theDoors_age = [47,50,55,49]



theDoors = pd.DataFrame({"Name":theDoors_names, "Age":theDoors_age})



# Adding a column
theDoors['childAge'] = [12,12,4,6]


# displaying the names of columns
print(theDoors.columns)


theDoors['fatherHoodAge'] = theDoors.Age - theDoors.childAge


print(theDoors)

# printing a column
print(theDoors.Age)
print(theDoors.childAge)
