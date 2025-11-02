#!/usr/bin/env python
# coding: utf-8

# Question 1

# (i) Calculate the average temperature value (index 9) for the entire dataset. Note the temperature 
# values have been normalized by dividing by 41. 

# In[14]:


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

# In[15]:


import numpy as np   # import Numpy as np (always do this when importing numpy)

def rentNum(data):  # define function, data is the input that will be given from main function
    numhol1 = data[:,6] == 1 # he said index 5 but looking at excel, it's column 7, so I used index 6?????
    numhol0 = data[:,6] == 0 # get all rows where column 7 is equal to 0
    rentnum = data[:,16]     # put rental users (last column??) into variable rentnum
    print(np.mean(rentnum[numhol1])) ## print mean of rental users that are equal to numhol1(holiday ==1)
    print(np.mean(rentnum[numhol0])) ## print mean of rental users that are equal to numhol0(holiday ==0)
    
def main():
    data=np.genfromtxt("hour.csv", delimiter=",", skip_header = 1) ## read in data from hour.txt
    
    rentNum(data) #apply function
    
main() #call main


# (iii) Write NumPy code that will print out the total number of casual users for each month of the 
# year. You would expect to see an increase in the number of casual users over the summer 
# months and a decline for the winter months. 

# In[16]:


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

# In[13]:


###didn't get same values as him, no idea where I went wrong??

import numpy as np   # import Numpy as np (always do this when importing numpy)

def tempNum(data):  # define function, data is the input that will be given from main function
    tempN = data[:,10].astype(float)*41  # convert temp
    rentN = data[:,16]       # get rent column
    for i in range(0,40,5):
        print("For temp in range", i+1, i+5, "the mean number of rental users was", np.mean(rentN[np.where((tempN>=i+1)&(tempN<=i+5))]))  
    
def main():
    data=np.genfromtxt("hour.csv", delimiter=",", skip_header = 1) ## read in data from hour.txt, skip row 0, start at 1
    
    tempNum(data) #apply function
    
main() #call main

