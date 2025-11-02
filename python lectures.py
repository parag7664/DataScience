#!/usr/bin/env python
# coding: utf-8

# In[1]:


print("hi python")


# In[3]:


""" 
hi comment

"""


# In[4]:


import keyword
keywords = keyword.kwlist
print(keywords)


# In[5]:


print('parag')
print('soni')


# In[6]:


print('parag', end = " ")
print('soni')


# In[8]:


name = 'parag'
print(name)
type(name)


# In[12]:


dist = float(input("enter distance in KM"))
print("dist in miles:", dist*0.6214)


# In[11]:


()


# In[14]:


first_name = input("enter first name")
last_name = input("enter last name")
grade_1 = int(input("enter grade for sub 1:"))
grade_2 = int(input("enter grade for sub 2:"))
grade_3 = int(input("enter grade for sub 3:"))
avg = (grade_1+grade_2+grade_3)/3
print(first_name, " ", last_name, "average is : ", avg )


# In[15]:


cls_A = float(65)
cls_B = float(55)
cls_C = float(47)
sales_A = int(input("class a tickets sold:"))
sales_B = int(input("class B tickets sold:"))
sales_C = int(input("class C tickets sold:"))
total = (sales_A*cls_A)+(sales_B*cls_B)+(sales_C*cls_C)
print("total income", total)


# In[18]:


weight = float(input("enter height :"))
height = float(input("enter weight :"))
bmi = (weight/height**2)*703
print('bmi is', bmi)


# In[22]:


sales =5000
discount = 0
if sales >5000:
    discount = 20
elif sales<5000 and sales > 2500:
    discount = 10
else:
    discount = 5
print(discount)


# In[25]:


num = 10
while num >4 and num<30:
    print (num)
    num = num +1


# In[27]:


number = 7
guess = int(input("enter a number between 1-10"))

while guess!=number:
    guess = int(input("enter a number between 1-10"))
print("correct guess")


# In[32]:


for num in [1,2,3,4,5]:
    for y in [1,2,3,4,5]:
        print(num)


# In[49]:



hi = "abc" #global
hey = "bcd"

def function1():
    print(hi)
    hiii = "222" #local
    print(hiii)
def main():
    print("jjjjllooooo")
    pass 
    """pass is a null statement ****""" 
    function1()
main()


# In[52]:


def main():
    length = 20
    width = 30
    area(length, width)

def area(l,w):
    area = l*w
    print("area ::", area)
    
main()


# In[58]:


globalvariable = 10

def main():
    print("globalvariable :: ",globalvariable)
    abc()
    print("globalvariable :: ",globalvariable)
    
def abc():
    #globalvariable = 12
    global globalvariable
    globalvariable = 15
    #print("globalvariable :: ",globalvariable)
          
main()


# In[65]:


num = 1
while(num<=20):
    print(num)
    num += 1
    
for i in range(1,20,3):
    print(i)


# In[69]:


number = int(input("enter a number > 0 : "))
while number<=0:
    number = int(input("enter a number > 0 : "))
    
for i in range(0,number+1):
    print(i)


# In[72]:


def tenTable(x):
    for i in range(11):
        print("x*",i,"=",x*i)

tenTable(10)


# In[74]:


start = int(input("Please enter first number"))
stop = int(input("Please enter second number"))
for num in range(start, stop+1, 1):
    if num%2 == 0:
        print (num, "is even")
    else:
        print (num, "is odd")


# In[75]:


num = int(input("Please enter first number"))
limit = int(input("Please enter second number"))
for i in range(0,limit+1):
    print(num,"*",i,"=",num*i)


# In[78]:


for i in range (0,6):
        print("*"*5)


# In[79]:


star = int(input("Please enter star number"))
lines = int(input("Please enter lines number"))
for i in range(0,lines):
    print("*"*star)


# In[86]:


a = int(input("Please enter number"))
for i in range(1,a+1):
    print(str(i)*i)
        


# In[87]:


num1 = int(input("Please enter first number"))
num2 = int(input("Please enter second number"))
operator = int(input("""Enter the operation tp perform : \n
1. Addition \n
2. Subtraction \n
3. Multiplication \n
4. Division \n"""))
if(operator==1):
    print("\n Addition of {} and {} is {}".format(num1, num2, num1+num2))
elif(operator==2):
    print("\n Subtraction of {} and {} is {}".format(num1, num2, num1-num2))
elif(operator==3):
    print("\n Multiplicaion of {} and {} is {}".format(num1, num2, num1*num2))
elif(operator==4):
    print("\n Division of {} and {} is {}".format(num1, num2, num1/num2))
else:
    print("Wrong choice")


# In[88]:


flag = "y"
while flag.lower()=='y':
    num1 = int(input("Please enter first number"))
    num2 = int(input("Please enter second number"))
    operator = int(input("""Enter the operation tp perform : \n
    1. Addition \n
    2. Subtraction \n
    3. Multiplication \n
    4. Division \n"""))
    if(operator==1):
        print("\n Addition of {} and {} is {}".format(num1, num2, num1+num2))
        flag = input("would you like to perform more operation? y/n \n")
    elif(operator==2):
        print("\n Subtraction of {} and {} is {}".format(num1, num2, num1-num2))
        flag = input("would you like to perform more operation? y/n \n")
    elif(operator==3):
        print("\n Multiplicaion of {} and {} is {}".format(num1, num2, num1*num2))
        flag = input("would you like to perform more operation? y/n \n")
    elif(operator==4):
        print("\n Division of {} and {} is {}".format(num1, num2, num1/num2))
        flag = input("would you like to perform more operation? y/n \n")
    else:
        print("Wrong choice")
        flag = input("would you like to perform more operation? y/n \n")


# In[94]:


import random
print(random.randint(9,10))


# In[100]:


import math
c =10
#math.log10(c)
#math.cos(c)
math.hypot(6,8)


# In[109]:


# custom module

def c2f(c):
    f = (9/5)*c+32
    return f

def f2c(f):
    c= (5/9)*f-32
    return c
def main(): #can remove this
    print(c2f(47))
    print(f2c(76))

"""
When a Python interpreter reads a Python file, it first sets a few special variables. Then it executes the code from the file.

One of those variables is called __name__.

"""
if __name__=='__main__':
    main()


# In[108]:


import os
import sys
sys.path.insert(0,os.path.abspath('users/psoni/desktop'))
import abc
abc.c2f(33)
get_ipython().run_line_magic('run', 'abc.py')


# In[152]:


myList = [1,2.3,"parag"]#heterogeneous ds

list=[1,3,4,5,6]
print(list[2])
print(list[-2])
size = len(list)

#numbers=range(4)
for num in list:
    print(num)
for num in range(len(list)):
    print()
last_index = len(list)-1
for num in range(last_index)


# In[144]:


limit = int(input("enter limit:"))
mylist =range(limit+1)
print([(mylist)])

# print(limit)
for i in mylist:
    print(i)


# In[155]:


x = int(input("enter limit:"))
mylist = range(x+1)

i = 1
while i<=x:
    print(mylist[-i])
    i+=1


# In[ ]:




