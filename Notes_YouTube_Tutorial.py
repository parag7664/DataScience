#!/usr/bin/env python
# coding: utf-8

# In[1]:


print("Hello World") # print string


# https://www.youtube.com/watch?v=kqtD5dpn9C8

# In[2]:


# Variables store data in computer's memory
age = 20 # create variable age
print(age) # no need for quotes here


# In[3]:


# Python follows commands in order so 30 will be printed here 
age = 20 
age = 30
print(age)


# In[4]:


# types of data called primitive or basic types
age = 20 # 20 is an integer
price = 19.95 # 19.95 is a float (decimal number)
first_name = "Mosh" # this is a string
is_online = False # this a boolean, must have capital 'F' as python is case sensitive


# In[5]:


# receiving input
input("What is your name? ") #takes input and prints it out (including the space after the ?)


# In[6]:


name = input("What is your name? ") 
print("Hello " + name)


# In[7]:


birth_year = input("Enter your birth year: ") # EVEN THOUGH INPUT IS NUMBER THIS WILL BE READ AS A STRING
age = 2021 - birth_year # 
print(age) #####ERROR - PYTHON CAN'T PRINT AN INTEGER WITH A STRING


# In[9]:


## Must coerce string inout into a numeric value for the above code to run
birth_year = int(input("Enter your birth year: ")) # change string to integer (or float if expecting decimals)
age = 2021 - birth_year 
print(age) 


# In[ ]:


# functions for converting data types
int()    # convert to integer
float()  # convert to float 
bool()   # convert to boolean
str()    # convert to string


# In[13]:


First = float(input("First: "))
Second = float(input("Second: "))
sum = First + Second # could also call float value here if you wanted
## Python can only concatenate same data types 
## so must convert output to string to be concatenated with the string "Sum: " in print
print("Sum: " + str(sum))


# In[ ]:


## objects in Python are like objects in the real world
## string functions
## when a function is specific to a particular object (string object, integre object, float object etc.,
## we refer to those functions as methods
## print is not specific about what type of object it gets so it's not a method, it's a general function

course = "Python for Beginners" # course variable is storing a string object
course. # in Spyder, the dot will bring up a list of functions you can use with your string variable "course"


# In[15]:


course = "Python for Beginners" # course variable is storing a string object
print(course.upper()) # this does not alter the variable course as it is immutable, just outputs it in uppercase letters
print(course.lower()) # prints variable course in lowercase letters
print(course.find("y")) # prints index of y letter in variable course
## Python for Beginners - this is the content of our variable
## 01234567............ - Here the index of Y is 0 as python index starts at 0
print(course.find("Y")) # this will print -1 as we don't have an uppercase Y in variable course
print(course.find("for")) # this will print the index number where 'for' starts
print("Python" in course) # this will return boolean value True
print(course.replace("for", "4")) # this prints out Python 4 Beginners but doesn't change original variable
print(course.replace("x", "4")) # just prints outs original varaible as there is no 'x' in variable course
print(course)


# In[18]:


## Arithmetic Operators
print(10 + 3)
print(10 - 3)
print(10 * 3)
print(10 / 3) # gives float number with decimal point
print(10 // 3) # gives whole integer value
print(10 % 3) # returns remainder of 10 / 3
print(10 ** 3) # exponent operators

## augmented operators
x = 10    # give variable x a value of 10
print(x)
x = x + 3  # add 3 to x, this will change value of variable x
print(x)
x += 3 # identical to code above but this is called an augmented operator
print(x)
x -= 3
print(x)
x *= 3
print(x)


# In[20]:


## operator precedence
x = 10 + 3 * 2 ## follows operator precedence, multiplication first, then addition
print(x)
x = (10 + 3)* 2 # what's in brackets will be computed first
print(x)


# In[22]:


## comparison operators
x = 3 > 2     # is 3 greater than 2?
print(x)

x = 3 >= 2    # is 3 greater than or equal to 2?
print(x)

x = 3 ==2     # is 3 equal to 2? Don't confuse assignment operator '=' with equals sign '=='
print(x)

x = 3 < 2     # is 3 less than 2?
print(x)

x = 3 <= 2    # is 3 less than or equal to 2?
print(x)

x = 3 != 2    # is 3 not equal to 2?
print(x)


# In[24]:


## logical operators - In python, we have 3 logical operators
price = 25
## must satisfy both conditions
print(price > 10 and price < 30) # print True if variable 'price' is graeter than 10 AND less than 30
## must satisfy at least one condition
print(price > 10 or price ==5) # print True if price greater than 10 OR is equal to 5
## must not satisfy condition, inverses value of condition
print(not price > 10)


# In[27]:


## Conditions using for loops
temperature = 35
if temperature > 30:
    print("It's a hot day") # use double quotes "" here if there's an apostrophe in your string (python gets confused)
    print("Drink plenty of water")
    
temperature = 25 ## this prints nothing as condition is not met
if temperature > 30:
    print("It's a hot day") # use double quotes "" here if there's an apostrophe in your string (python gets confused)
    print("Drink plenty of water")
    
temperature = 25 ## nothing happens here as condition is not met
if temperature > 30:
    print("It's a hot day") # use double quotes "" here if there's an apostrophe in your string (python gets confused)
    print("Drink plenty of water")
elif temperature > 20:   # python reads conditions in order, if first is not met, it move to second condition so no need to write between 20 and 30 here
    print("It's a nice day")
else:
    print("It's cold") # if first 2 conditions aren't met, this is printed
print("Done") # this will be printed regardless as it's not related to a condition


# In[31]:


weight = float(input("Weight: ")) # don't forget to convert to float
unit = input("(k)g or (L)bs: ") # type K if you put your weight kilograms or L if in pounds
if unit.upper == "K": # use .upper here in case person types a lowercase by accident
    converted = weight / 0.45  # convert to pounds
    print("Weight in Lbs: " + str(converted)) # don't forget to convert to string
else: 
    converted = weight * 0.45 # convert lbs to kilograms
    print("Weight in Kg: " + str(converted))


# In[32]:


## while loops to repeat block of code
i = 1
while i <= 5:
    print(i)
    i = i + 1 ####### must increment while loops as they will run forever while i <= 5


# In[34]:


## while loops to repeat block of code
i = 1
while i <= 5:
    print(i * "*") ## you can use a multiplication operator here for integers and strings
    i = i + 1


# In[36]:


## lists represents a list of objects (basic types)
names = ["Jimmy", "Joe", "Bob", "Beer"]
print(names[0]) # prints first name in list
print(names[-1]) # prints last object in list
print(names[-2]) # prints second last object in list
names[0] = "Jon" # changes value of first object in list to Jon using assigment operator
print(names[0:3]) # returns first element and elements up to, but not including*** 3rd element


# In[37]:


numbers = [1, 2, 3, 4, 5]
print(numbers)
numbers.append(6)# add 6 to the end
print(numbers)
numbers.insert(0, -1) # put -1 before index 0
print(numbers)
numbers.remove(3) # take out value 3
print(numbers)
numbers.clear() # clear the list
print(numbers)
print(1 in numbers) # print true if 1 is in list numbers
print(len(numbers)) # print number of elements in list


# In[38]:


## for loops versus while loops
numbers = [1, 2, 3, 4, 5]
for item in numbers:
    print(item)       ## nice easy iteration with less code
    
i = 0
while i < len(numbers):  ## longer code, a bit messier
    print(numbers[i])
    i = i + 1


# In[45]:


## range function
numbers = range(5)
print(numbers) # this will print out range(0, 5), not 0, 1, 2, 3, 4 as range(0, 5) is default representation
for number in numbers:
    print(number) # this will print out 0, 1, 2, 3, 4 (as index starts at 0)
    
numbers2 = range(5, 10)
for number in numbers2:
    print(number) # this will print 5, 6, 7, 8, 9, but doesn't include 10
    
numbers3 = range(5, 10, 2) # range from 5 up to but not including 10 in 2 steps
for number in numbers3:
    print(number) # this will print 5, 7, 9
    
## if you just want to print numbers, you don't need to specify variable
for number in range(5):
    print(number)


# In[ ]:


## Tuples are similar to lists but are immutable, can't be reassigned
numbers = (1, 2, 3, 3) # use parenthesis to define a tuple and square brackets for list
numbers[0] = 10  ##ERROR CAN'T REASSIGN ELEMENT AS IT'S IN A TUPLE

