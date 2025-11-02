#!/usr/bin/env python
# coding: utf-8

# Question 1

# In[18]:


## scrap, can ignore
import numpy as np 
data = np.genfromtxt("hour.csv", delimiter=",", skip_header = 1) ## read in data from hour.txt
data.size
data.shape[0] # number of rows
data.shape[1] # number of columns
print(data[:,6].size)
numhol1 = data[:,6] == 1
numhol0 = data[:,6] == 0
rentnum = data[:,-1]
print(np.sum(numhol1))
print(np.sum(numhol0))
print(np.sum(rentnum))
print(np.mean(rentnum[numhol1]))
print(np.mean(rentnum[numhol0]))


listmnt = []
mnt = data[:,4]
cas = data[:,14]
jan=cas[np.where(mnt==1)]
print(jan)
print(jan.size)

tempN = (data[:,10])*41
rentN = data[:,15]
tempN[5:,]


# (i) Calculate the average temperature value (index 9) for the entire dataset. Note the temperature 
# values have been normalized by dividing by 41. 

# In[2]:


import numpy as np   # import Numpy as np (always do this when importing numpy)
## import os
## os.chdir("C:/Users/...")

def getavgtemp(data):  # define function, data is the input that will be given from main function
    allavg=np.mean(data, axis=0) # get the mean of all columns in the data set
    print(allavg[9]*41) # as temp was divided by 41, multiply by 41 to get temp
    
def main():
    data=np.genfromtxt("hour.csv", delimiter=",", skip_header = 1) ## read in data from hour.txt
    
    getavgtemp(data) #apply function
    
main() #call main


# (ii) Print out the average number of rental users for all days classified as holidays as well as the 
# average for all days classified as non-holidays. (Note holidays =1 and non-holidays = 0). 
# Holidays attribute is stored at index 5.

# In[29]:


import numpy as np   # import Numpy as np (always do this when importing numpy)

def rentNum(data):  # define function, data is the input that will be given from main function
    numhol1 = data[:,6] == 1 # he said index 5 but looking at excel, it's column 7, so I used index 6?????
    numhol0 = data[:,6] == 0 # get all rows where column 7 is equal to 0
    rentnum = data[:,-1]     # put rental users (last column??) into variable rentnum
    print(np.mean(rentnum[numhol1])) ## print mean of rental users that are equal to numhol1(holiday ==1)
    print(np.mean(rentnum[numhol0])) ## print mean of rental users that are equal to numhol0(holiday ==0)
    
def main():
    data=np.genfromtxt("hour.csv", delimiter=",", skip_header = 1) ## read in data from hour.txt
    
    rentNum(data) #apply function
    
main() #call main


# (iii) Write NumPy code that will print out the total number of casual users for each month of the 
# year. You would expect to see an increase in the number of casual users over the summer 
# months and a decline for the winter months. 

# In[55]:


import numpy as np   # import Numpy as np (always do this when importing numpy)

def monthNum(data):  # define function, data is the input that will be given from main function
    mnt = data[:,4]   ## month column
    cas = data[:,14]  ## casual user column
    jan=cas[np.where(mnt==1)]    ## where casual column == value in month column, put in variable jan
    feb=cas[np.where(mnt==2)]
    mar=cas[np.where(mnt==3)]
    apr=cas[np.where(mnt==4)]
    may=cas[np.where(mnt==5)]
    jun=cas[np.where(mnt==6)]
    jul=cas[np.where(mnt==7)]
    aug=cas[np.where(mnt==8)]
    sep=cas[np.where(mnt==9)]
    oct=cas[np.where(mnt==10)]
    nov=cas[np.where(mnt==11)]
    dec=cas[np.where(mnt==12)]
    
    print("Total users for month Jan is", int(np.sum(jan))) ## print sum of jan, used int to round off here
    print(np.sum(feb))
    print(np.sum(mar))
    print(np.sum(apr))
    print(np.sum(may))
    print(np.sum(jun))
    print(np.sum(jul))
    print(np.sum(aug))
    print(np.sum(sep))
    print(np.sum(oct))
    print(np.sum(nov))
    print(np.sum(dec))
    
def main():
    data=np.genfromtxt("hour.csv", delimiter=",", skip_header = 1) ## read in data from hour.txt, skip row 0, start at 1
    
    monthNum(data) #apply function
    
main() #call main


# In[5]:


###trying for loop hacky crap!!!

import numpy as np   # import Numpy as np (always do this when importing numpy)

def monthNum(data):  # define function, data is the input that will be given from main function
    mnt = data[:,4]
    cas = data[:,14]
    months = ['Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sept','Oct','Nov','Dec']
    j = 1
    while j <= 12:
        print("Total users for", months[j-1], "is", int(np.sum(cas[np.where(mnt==j)])))
        j += 1
    
def main():
    data=np.genfromtxt("hour.csv", delimiter=",", skip_header = 1) ## read in data from hour.txt, skip row 0, start at 1
    
    monthNum(data) #apply function
    
main() #call main


# (iv) We will now analyse the relationship between temperature and the number of rental users. Your 
# code should work out the average number of rental users for the following temperature ranges. 

# In[43]:


###didn't get same values as him, no idea where I went wrong??

import numpy as np   # import Numpy as np (always do this when importing numpy)

def tempNum(data):  # define function, data is the input that will be given from main function
    tempN = (data[:,10])*41  # convert temp
    int_tempN = tempN.astype(int)
    rentN = data[:,15]       # get rent column
    print(np.mean(rentN[np.where(int_tempN<=5)]))
    print(np.mean(rentN[np.where((int_tempN>5) & (int_tempN <= 10))]))
    print(np.mean(rentN[np.where((int_tempN>10) & (int_tempN <= 15))]))
    print(np.mean(rentN[np.where((int_tempN>15) & (int_tempN <= 20))]))
    print(np.mean(rentN[np.where((int_tempN>20) & (int_tempN <= 25))]))
    print(np.mean(rentN[np.where((int_tempN>25) & (int_tempN <= 30))]))
    print(np.mean(rentN[np.where((int_tempN>30) & (int_tempN <= 35))]))
    print(np.mean(rentN[np.where((int_tempN>35) & (int_tempN <= 40))]))
    
    
def main():
    data=np.genfromtxt("hour.csv", delimiter=",", skip_header = 1) ## read in data from hour.txt, skip row 0, start at 1
    
    tempNum(data) #apply function
    
main() #call main


# In[44]:


###trying for loop hacky crap!!!

import numpy as np   # import Numpy as np (always do this when importing numpy)

def tempNum(data):  # define function, data is the input that will be given from main function
    tempN = (data[:,10])*41  # convert temp
    int_tempN = tempN.astype(int)
    rentN = data[:,15]       # get rent column
    print(int_tempN[10:,])
    
    
def main():
    data=np.genfromtxt("hour.csv", delimiter=",", skip_header = 1) ## read in data from hour.txt, skip row 0, start at 1
    
    tempNum(data) #apply function
    
main() #call main


# In[ ]:




