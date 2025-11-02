#Install packages
install.packages("ggplot2")
install.packages("dplyr")
install.packages("RColorBrewer")
install.packages("lubridate")
install.packages("tidyr")
install.packages("tidyverse")
install.packages("leaflet")
install.packages("hrbrthemes")
install.packages("png")
install.packages("gifski")
install.packages("gganimate")
install.packages("zoo")
install.packages("forecast")
install.packages("plotly")


#Load packages
library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(lubridate)
library(tidyr)
library(tidyverse)
library(leaflet)
library(hrbrthemes)
library(png)
library(gifski)
library(gganimate)
library(zoo)
library(forecast)
library(plotly)

###########################################################################################################################
#setting directory and  reading file
#setwd("D:/Sem 2/Data Analytics and Visualisation/Assignment 1")
df<-read.csv('AWS_Honeypot_marx-geo.csv',sep=',',stringsAsFactors=F)
str(df)
############################################################################################################################
#EDA
#factorising features of dataset
data1 <-
  df %>% 
  mutate(
    datetime = datetime %>% parse_date_time(order = "%m/%d/%y %H:%M"),
    host= host %>% as.factor,
    proto = proto %>% as.factor,
    country = country %>% as.factor %>% fct_infreq(), # changes order by number of observations with each level (largest first)
    country_by_group = country %>% fct_lump(n = 10),#to "lump" all the infrequent countries into one factor, "other." n is the number of levels we want can use prop = .1 too for proportion
    srcstr = srcstr %>% as.factor %>% fct_infreq(), # changes order by number of observations with each level (largest first)
    ip_by_group = srcstr %>% fct_lump(n = 10)) 

# str(data1)
# View(data1)
# summary(data1)

data1$datetime <- sort(data1$datetime) # need to sort as dates are a bit shuffled

# data cleanup - missing geo locations
clean_data <- data.frame(data1 %>% filter(!is.na(latitude) & !is.na(longitude)))

# merging column X (which has corrected value for latitude)and latitude where latitude is provided with wrong value  >90
res <- which(clean_data$latitude>90)
clean_data$latitude[res] <- clean_data$X[res]
clean_data$X <- NULL

# creating a dataframe with the selective features for visualisation
df1 <- clean_data %>% dplyr::select(datetime, host, proto, type, srcstr, country, locale, latitude, longitude, country_by_group, ip_by_group)
View(df1)

#segregating datetime in  year, Month, Day, hour, Minute and seconds
df1$time <- format(df1$datetime,"%H:%M:%S")
df1$date <- format(df1$datetime,"%Y:%m:%d")

df1 <- separate(df1, "date", c("Year", "Month", "Day"), sep = ":")
df1 <- separate(df1, "time", c("Hour", "Minute", "Seconds"), sep = ":")

testdata <- df1
View(testdata)
str(testdata)
#subsetting data to test as there are around 4.5 lack of rows in the dataset
data_subset = df1[c(0:10000),]
View(data_subset)
##################################################################################################################################

# evaluating time difference in minutes for all the attacks
time_diff_min <- round(difftime(testdata$datetime,lag((testdata$datetime), default = first(testdata$datetime)), units = "mins"),3)  
testdata$time_diff_in_minute <- time_diff_min

diff_sec <- difftime(testdata$datetime,lag((testdata$datetime), default = first(testdata$datetime)), units = "sec")
testdata$time_diff_in_sec <- diff_sec


###########################################################################################################################################
#Plot 1
#MAP - Geolocation of all the attacks around the globe
data_subset %>%
  leaflet() %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "World Imagery") %>%
  addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
  addLayersControl(baseGroups = c("Toner Lite", "World Imagery")) %>%
  addMarkers(label = data_subset$host,
             clusterOptions = markerClusterOptions(),
             popup = ifelse(data_subset$type==3,
                            "Type 3",
                            "Type 8")) %>%
  setView(lat = 28.5500, lng = 115.9333, zoom = 11) %>%
  addMiniMap(
    toggleDisplay = TRUE,
    tiles = providers$Stamen.TonerLite
  )

###########################################################################################################################################
#Plot 2
#Barplot for Average Number of attacks each month
#clearly attacks at the end of year is more'

df2 <- testdata %>%
  group_by(Month, Day) %>% count()


plot <- ggplot(df2, aes(x = Month, y=n, color = Month)) + 
  geom_boxplot(outlier.size=5) +
  geom_jitter(size = 2)+
  ylim(1000,3500)+
  geom_hline(aes(yintercept = mean(n)), col= "red")+
  labs(x = "Month",
       y = "Number of attacks",
       title = "Average Attacks Per Day in each month Month",
       colour="Month") +
  hrbrthemes::theme_modern_rc()
ggplotly(plot)

##########################################################################################################################################
#Plot 3
# Graph 3
# Top countries with highest number of attacks categorised by MONTH OF ATTACK
ggplot(testdata) +
  aes(x = country_by_group, fill = Month) +
  geom_bar(position = "dodge") +
  scale_fill_brewer(palette = "Dark2", direction = 1) +
  labs(
    x = "Country",
    y = "Number of attacks",
    title = "Countries With Highest Number of attacks  each month"
  ) +
  hrbrthemes::theme_modern_rc()

#add percentages to our data
data <- testdata %>%
  group_by(Month, country_by_group) %>% count() %>%
  mutate(pct = n / sum(n), pct = scales::percent(pct))

# plot_ly function call for more interactive graph
fig <- plot_ly(x = ~data$country_by_group, y = ~data$n, type = 'bar', text = ~data$pct, name = ~data$Month, color = ~data$Month)
fig <- fig %>% layout(title = 'Countries With Highest Number of attacks', plot_bgcolor = "#e5ecf6", xaxis = list(title = 'Country'), 
       yaxis = list(title = 'Number of attacks'), legend = list(title=list(text='<b> Month </b>')))

fig
##########################################################################################################################################
#Plot 4
#attacks per day for each month for top countries

a <- ggplot(df2, aes(x = Day, y = n, group = Month, color = Month)) + 
  geom_line()+
  geom_point() +
  scale_fill_brewer(palette = "Paired")+
  geom_hline(aes(yintercept = 13463), col= "red")+# marking a line at highest number of attack
  labs(x = "Day",
       y = "Number of attacks",
       title = "Timeline of attacks per day for each month",
       colour="Month") +
  hrbrthemes::theme_modern_rc()
  
ggplotly(a)

################################################################################################
#Plot 5
# Number of attacks country wise on each day every month
df3 <- testdata %>%
  group_by(country, Day, Month) %>% count()


df3 %>%
  filter(country %in% c("China", "United States", "Japan", "Iran", "Taiwan", "Netherlands", "India", "South Korea", "Vietnam", "Russia")) %>%
  ggplot()+
  aes(x = Day, y = n, group = factor(country), color = factor(country)) + 
  geom_line()+
  geom_point() +
  scale_fill_brewer(palette = "Paired")+
  labs(x = "Day",
       y = "Number of attacks",
       title = "Timeline of attacks per day country wise",
       colour="Country") +
  theme_classic()+
  facet_wrap(vars(Month), scales = "free")

###############################################################################################
#Plot 6 
#Top risk countries and their source of attack for each month

#Plotly graph
df4 <- testdata %>%
  group_by(ip_by_group, Month) %>% count() %>%
  mutate(pct = n / sum(n), pct = scales::percent(pct))

fig <- plot_ly(x = ~df4$ip_by_group, y = ~df4$n, type = 'bar', text = ~df4$pct, name = ~df4$Month, color = ~df4$Month)
fig <- fig %>%layout(title = "Top IP's and their month of attack", plot_bgcolor = "#e5ecf6", xaxis = list(title = 'Source(IP)'), 
                     yaxis = list(title = 'Number of attacks'), legend = list(title=list(text='<b> Month </b>')))
fig


p <- ggplot(testdata) +
  aes(x = ip_by_group, fill = Month) +
  geom_bar(position = "dodge") +
  scale_fill_viridis_d(option = "inferno", direction = 1) +
  labs(
    x = "Source of Attacks(IP)",
    title = "Top risk countries and their source of attack for each month",

  ) +
  theme_dark() +
  facet_wrap(vars(country_by_group), scales = "free")

ggplotly(p)
#############################################################################################
#Plot 8
# Forecasting attacks for next month
attacks <- ts(df2$n, frequency = 30, start = 03)
plot(attacks)
attack_ets <- ses(attacks,
                  alpha = .2,
                  h = 80)
autoplot(attack_ets)
summary(attack_ets)
