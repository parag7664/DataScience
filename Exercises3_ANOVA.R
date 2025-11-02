#######################################

########   Exercises 3 ANOVA  #########

#######################################

library(tidyverse)
library(here)
library(outliers)


####  Q7 Kenton Foods #######

kenton <- read_tsv("Kenton_Foods.txt")
kenton$Package <- as.factor(kenton$Package)

windows(5,5)
plot(kenton$Cases~ kenton$Package)
fligner.test(kenton$Cases~ kenton$Package)

model <- aov(kenton$Cases ~ kenton$Package)
summary(model)
summary.lm(model) 
#contrasts(kenton$Batch) <- contr.sum 
windows(5,5)
plot(TukeyHSD((model)))

kenton %>% group_by(Package) %>% 
  summarise(mean_Cases = mean(Cases), sd_Cases = sd(Cases))

##############  Q8 Vitamin C ############
vitaminC <- read_xlsx("vitaminC.xlsx")
vitaminC$variety <- as.factor(vitaminC$variety)

windows(5,5)
plot(content~ variety, data=vitaminC)
leveneTest(vitaminC$content~ vitaminC$variety)   


model <- aov(content ~ variety, data=vitaminC)
summary(model)
summary.lm(model) 
windows(5,5)
plot(TukeyHSD((model)))

vitaminC %>% group_by(variety) %>% 
  summarise(mean_content = mean(content), sd_content = sd(content))



####  Q9  ########

## read the data into a tibble called drinks
drinks <- read_tsv("D:/Sem 2/statistical Data analysis/Exercise 3/soft_drink.txt")

## convert the variable Display to a factor
drinks$Display<-as.factor(drinks$Display) 

## calculate summary statistics for the sales of each type of display
drinks %>% group_by(Display) %>% 
            summarise(mean_Increase = mean(Increase), sd_Increase = sd(Increase),
                      max_Increase = max(Increase), min_Increase = min(Increase))


## plot the distribution of the percentage increase in sales for each Display type
windows(5,5)
plot(drinks$Increase~drinks$Display, ylab = "Increase" )

## check distributions of each variable
  
  windows(10,5)
  par(mfrow = c(1,3))
  drinks %>%
    filter(Display == "3") %>%
    with(hist(Increase, main = "Histogram for Design 3" ) )
  

## test for equality of variances (homogeniety of variance assumption)
fligner.test(drinks$Increase~drinks$Display)


## Fit the ANOVA model
model_treat<- aov(drinks$Increase~drinks$Display) 
summary(model_treat)      #view drinks of ANOVA model (ANOVA table)
summary.lm(model_treat)   # view treatment effect estimates



#### Examine the pairwise effects  ###
TukeyHSD(model_treat)
windows(5,5)
plot(TukeyHSD(model_treat))

### Model validation plots ###
par(mfrow = c(2,2)) #split graphics window into 2 rows 2 columns
plot(model_treat)  # validation plots



### Q10 ####################


## Import data

bricks <- read_tsv("D:/Sem 2/statistical Data analysis/Exercise 3/brick.txt")

# Basic summary statistics
bricks %>% group_by(Temperature) %>% 
  summarise(mean_Density = mean(Density), sd_Density = sd(Density))


## Change Temperature to a Factor
bricks$Temperature<-as.factor(bricks$Temperature)


## plot the distribution of the Density for each Temperature 
boxplot(bricks$Density~bricks$Temperature, ylab = "Density")

## check distributions of each variable

windows(10,10)
par(mfrow = c(2,2))
bricks %>%
        filter(Temperature == "175") %>%
        with(hist(Density))
        

#  Check outlier in the Density variable - Potential outlier not significant
## when using all treatments
grubbs.test(bricks$Density)

#  Check outlier in the Density variable for the treatment where 
## Temperature = 175 - Potential outlier is significant within 
## the 175 treatment where Temp = 175. Bear this in mind when assessing the model
grubbs.test(bricks$Density[bricks$Temperature=="175"])

## Calculate the variance of the Densitys for each Temperature type
bricks %>% group_by(Temperature) %>% 
  summarise( var_Density = var(Density))

## test for equality of variances (homogeniety of variance assumption)
fligner.test(bricks$Density~bricks$Temperature)


## Fit the ANOVA model
model_bricks<- aov(bricks$Density~bricks$Temperature) # define ANOVA model
summary(model_bricks)      #view summary of ANOVA model (ANOVA table)
summary.lm(model_bricks)   # view treatment effect estimates

#### Examine the pairwise effects  ###
TukeyHSD(model_bricks)
windows(5,5)
plot(TukeyHSD(model_bricks))

### Model validation plots ###
par(mfrow = c(2,2)) #split graphics window into 2 rows 2 columns
plot(model_bricks)  # validation plots

## diagnostic plots indicate that the outlier at observation 37 
## is highly influential - remove the outlier and rerun the analysis

bricks_rm <-bricks[-37,]

# check boxplots again
boxplot(bricks_rm$Density~bricks_rm$Temperature, ylab = "Density")

## test homogeniety of variance again 
fligner.test(bricks_rm$Density~bricks_rm$Temperature)


## Fit the ANOVA model
model_bricks_rm<- aov(bricks_rm$Density~bricks_rm$Temperature) # define ANOVA model
summary(model_bricks_rm)      #view bricks of ANOVA model (ANOVA table)
summary.lm(model_bricks_rm)   # view treatment effect estimates

#### Examine the pairwise effects  ###
TukeyHSD(model_bricks_rm)
windows(5,5)
plot(TukeyHSD(model_bricks_rm))

## Removing the outlier changes the results of the pairwise comparisons, since there 
## is no additional information regarding the outlier and it is a reasonable observation for 
## brick density, both  sets of results need to be written up.

### Model validation plots ###
par(mfrow = c(2,2)) #split graphics window into 2 rows 2 columns
plot(model_bricks_rm)  # validation plots
