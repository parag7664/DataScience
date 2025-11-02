# -*- coding: utf-8 -*-
"""

Solutions for NumPy and Pandas Exercises 

"""

import numpy as np
import pandas as pd


############ Solutiosn to Question 1 Rainfall #########################

def calculateTotalRainfall(data, startMonth, endMonth):
    
    montlyFilter =  (data[:, 1] >= startMonth) & (data[:, 1] <= endMonth) 
    monthlyData = data[montlyFilter]
    print ("Average total rainfall for month {} to month {} is {}.".format(startMonth, endMonth, np.mean(monthlyData[:,2])))
    
    
def rainfallSolutions():
    
    # load data
    corkData = np.genfromtxt("CorkRainfall.txt")
    dublinData = np.genfromtxt("DublinRainfall.txt")

    # Q1 (i) max and mean of ‘Most Rainfall in a Day’ value
    print (np.max(corkData[:,3]))
    print (np.mean(corkData[:,3]))

    # Q1 (ii) Total number of raindays for a specific year
    
    uniqueYears = np.unique(corkData[:,0])
    year = int(input("Please enter a year between {} and {}:  ".format(np.min(uniqueYears), np.max(uniqueYears)) ))

    # Extract all rows pertaining to selected year
    yearData = corkData[ corkData[:,0] == year]   
    print ("Total number of raindays ", np.sum(yearData[:, 4]))
    
    
    # Q1 (iii) Calculate wettest month of the year
    
    maxRainfall = 0.0
    wettestMonth = 1
    uniqueMonths = np.unique(corkData[:,1])
    
    for month in uniqueMonths:
        
        # Extract all rows for specific month
        monthData = corkData[corkData[:, 1] == month] 
        totalRainfall = np.sum(monthData[:,2])
        
        if totalRainfall > maxRainfall:
            maxRainfall = totalRainfall
            wettestMonth = month
    
    print ("Wettest month is {} with a total rainfall value of {}".format(wettestMonth, maxRainfall))
        
    
        
    # Q 1 (iv) Percentage raindays below theshold
    
    threshold = int(input("Please enter max theshold for number of raindays: "))
    thresholdData = corkData [ corkData[:, 4] <= threshold ]
    
    perctageBelowThreshold = (len(thresholdData)/len(corkData))*100
    print ("Percentage of raindays below threshold {}.".format(perctageBelowThreshold))
    
    
    # Q 1 (v) average ‘total rainfall’ value for the summer months (June, July and August) and the Autumn months (Sept, Oct, Nov)
        
    calculateTotalRainfall(corkData, 6, 8)   
    calculateTotalRainfall(corkData, 9, 11)

    
    # Q 1 (vi) Append dublin and cork data and calculate average raindays

    mergedData = np.append(corkData, dublinData, axis = 0)
    print ("Average number of rain days for Cork and Dublin is {}.".format(np.mean(mergedData[:,4])))
    np.savetxt("DublinCorkRainfall.csv", mergedData, fmt="%f", delimiter=",")
    



############ Solutions to Question 2 Bicycle Dataset #########################


def cacluateMeanUsersHoliday(data, dayType):

    dayTypeSubset = data[data[:,5]==dayType]    
    numUsers = dayTypeSubset[:, 15]
    return (np.mean(numUsers))


def calculateUsersPerMonth(data):
 
    for currentMonth in range(1,13):
        
        # Extract all rows for the current month (currentMonth)
        booleanRowsForMonth = (data[:, 3] == currentMonth)
        dataForMonth = data[booleanRowsForMonth]
    
        print ("Total users for month {} is {}".format(currentMonth, np.sum(dataForMonth[:,13])))
        
        


def analyseTemp(data, minValue, maxValue):
    
    # the temperature values stored in the array are multiplied by 41 
    higherTempCondition = (data[:,9]*41)>=minValue    
    lowerTempCondition = (data[:,9]*41)<=maxValue
    
    # extract all temperature value that satisfying boolean filters above
    subset = data[higherTempCondition & lowerTempCondition]

    # calculate mean number of casual users
    meanValue = np.mean(subset[:, 15])

    print ("For temp in range {} to {} the mean number of casual users was {}".format(minValue, maxValue, meanValue))






def bicycleDataSolutions():
    
    data = np.genfromtxt('bike.csv', delimiter=',')
    
    # Q2 (i) Get average tempature
    print ("Average Temperature is {}".format(np.mean(data[:,9]*41.0)))
    
    
    # Q2 (ii) 
    print ("Mean number of non-holiday users {}".format(cacluateMeanUsersHoliday(data, 0)))
    print ("Mean number of holiday users {}".format(cacluateMeanUsersHoliday(data, 1)))
 
    # Q2 (iii) Mean number of casual users per month
    calculateUsersPerMonth(data)
    
    # Q2 (iv) Mean number of casual users per month
    for temp in range(1, 40, 5):
        analyseTemp(data, temp, temp+4)
        



############ Solutiosn to Question 3 Shark Attack Dataset #########################


def calculateFatalAttacks(country, df):

    boolCountry= df["Country"]==country
    boolFatal = df["Fatal"]=='Y'
    boolNonFatal = df["Fatal"]=='N'
    
    # you will notice in the dataset that some entries in the fatality column
    # are recorded as UNKNOWN, n, F, etc. We ignore these entries. 
    
    fatalAttacks = df.loc[boolCountry & boolFatal]
    
    nonFatalAttacks = df.loc[boolCountry & boolNonFatal]
    
    if (len(fatalAttacks) > 0 and len(nonFatalAttacks) > 0):
        print ("Percentage of attacks that are fatal in ", country,": ", (len(fatalAttacks)*100.0)/(len(fatalAttacks)+len(nonFatalAttacks)))
    



def sharkDataSolutions():

    
    df = pd.read_csv('attacks.csv', encoding = "ISO-8859-1")

    # Solution to Q3 (i) (The location that has the highest number of attacks globally)
    print (df["Location"].value_counts().head(1))
    
    
    # Solution to Q3 (ii) (Print the six countries that experience the highest number of shark attacks)
    print (df["Country"].value_counts().head(6))
    
    # Solution to Q3 (iii) (Print the six countries that experience the highest number fatal of shark attacks)
    criteria1 = df["Fatal"]=='Y'
    dfB = df.loc[criteria1, "Country"]
    print (dfB.value_counts().head(6))
    
    
    # Solution to Q3 (iv) (Bodyboarder or Surfer)
    boolSurfAttacks = df["Activity"] == "Surfing"
    boolScubaAttacks = df["Activity"] == "Scuba diving"    
    
    print ("Number of attacks when surfing {}".format( len(df.loc[boolSurfAttacks])))    
    print ("Number of attacks when Scuba Diving {}".format( len(df.loc[boolScubaAttacks])))
    
    
    
    # Solution Q 3 (v) Of all recorded shark attacks, what percentage were fatal. 
    # you will notice in the dataset that some entries in the fatality column
    # are recorded as UNKNOWN, n, F, etc. We ignore these entries and only consider 
    # those rows that contain a valid Y or N. 
    
    fatalAttacks = df.loc[df["Fatal"]=='Y']
    nonfatalAttacks = df.loc[df["Fatal"]=='N']
    
    print ("Percentage of attacks that are fatal : {}".format((len(fatalAttacks)*100.0)/(len(nonfatalAttacks)+len(fatalAttacks))))
    
    # Solution Q3 (vi) Calculate for each country the percentage of fatal shark attacks. 
    
    allCountries =  pd.unique(df["Country"])
    
    for country in allCountries:
        calculateFatalAttacks(country, df)


def main():
    
    # Solutions to Question 1
    #rainfallSolutions()
    
    # Solutions to Question 2   
    #bicycleDataSolutions()
    
    # Solutions to Question 3   
    sharkDataSolutions()


if __name__ == "__main__":
    main()
