library(ggplot2)
library(dplyr)
library(RColorBrewer)
library(lubridate)
library(tidyr)
library("tidyverse")
install.packages("leaflet")
library(leaflet)


###########################################################################################################################
#setting directory and  reading file
setwd("D:/Sem 2/Data Analytics and Visualisation/Assignment 1")
df<-read.csv('AWS_Honeypot_marx-geo.csv',sep=',',stringsAsFactors=F)

############################################################################################################################
#EDA
#factorising
data1 <-
  df %>% 
  mutate(
    datetime = datetime %>% parse_date_time(order = "%m/%d/%y %H:%M"),
    host= host %>% as.factor,
    proto = proto %>% as.factor,
    country = country %>% as.factor %>% fct_infreq(), # changes order by number of observations with each level (largest first)
    country_by_group = country %>% fct_lump(n = 10),
    srcstr = srcstr %>% as.factor %>% fct_infreq(), # changes order by number of observations with each level (largest first)
    ip_by_group = srcstr %>% fct_lump(n = 10)) #to "lump" all the infrequent countries into one factor, "other." n is the number of levels we want can use prop = .1 too for proportion

str(data1)
View(data1)
summary(data1)

# data cleanup - missing geo locations
df_clean <- data.frame(data1 %>% filter(!is.na(latitude) & !is.na(longitude)))

df1 <- df_clean %>% dplyr::select(datetime, host, proto, type, srcstr, country, locale, latitude, longitude, X, country_by_group)
View(df_clean)
data_subset = df1[c(0:5000),]
View(data_subset)

# merging column X anfd lattitude where latitude is provided wrong meaning >90
res <- which(data_subset$latitude>90)
View(res)
data_subset$latitude[res] <- data_subset$X[res]
data_subset$X <- NULL

df2 <- df1 %>%
  count(srcstr, sort = TRUE)


testdata$time <- format(testdata$datetime,"%H:%M:%S")
testdata$date <- format(testdata$datetime,"%Y:%m:%d")

testdata %>% 
  mutate(week = week(datetime))

testdata <- separate(testdata, "date", c("Year", "Month", "Day"), sep = ":")
testdata <- separate(testdata, "time", c("Hour", "Minute", "Seconds"), sep = ":")
View(testdata)

testdata <- df_clean
#is.na(df_clean$latitude)

testdata$datetime <- sort(testdata$datetime) # need to sort as dates are a bit shuffled

##################################################################################################################################

# time difference in minutes and active time
time_diff_min <- round(difftime(testdata$datetime,lag((testdata$datetime), default = first(testdata$datetime)), units = "mins"),3)  
testdata$time_diff_active_time_in_minute <- time_diff_min

diff_sec <- difftime(testdata$datetime,lag((testdata$datetime), default = first(testdata$datetime)), units = "sec")
testdata$time_diff_in_sec <- diff_sec

###################################################################################################################################
#Not working for grouping IP
head(testdata %>%
  group_by(testdata$srcstr) %>%
  summarise(n = n(), sort(TRUE)), 10)


View(testdata)

#distribution of attacks across the year:
testdata %>% 
  ggplot(aes(datetime)) + 
  geom_freqpoly(binwidth = 86400) # 86400 seconds = 1 day

# within a single day:
testdata %>% 
  filter(datetime < ymd(20130403)) %>% 
  ggplot(aes(datetime)) + 
  geom_freqpoly(binwidth = 600) # 600 s = 10 minutes

install.packages("hrbrthemes")
library(hrbrthemes)

# timeline of attacks vs the time diferrence i.e. the difference pretween the former and later attack
ph <- ggplot(testdata, aes(datetime, testdata$time_diff_in_min)) +
  geom_line(aes(colour=testdata$country_by_group)) + 
  geom_point() +
  xlab("Time") +
  ylab("IP Active Time") +
  theme_ipsum() +
  theme(axis.text.x=element_text(angle=60, hjust=1)) 
ph

install.packages("png")
library('png')
install.packages("gifski")
library('gifski')
install.packages("gganimate")
library('gganimate')

gif <- animate(
  ggplot(testdata, aes(testdata$datetime, testdata$active_time_in_min)) +
    geom_point(size=2)+
    geom_line(aes(colour=testdata$Month))+
    transition_reveal(testdata$datetime)+
    labs(title = "ph vs time vs batch" , xlab = "Time", ylab = "PH"), duration = 20, fps = 20)
gif  

#plot(df_clean$datetime, df_clean$country)

#distribution of attacks across the year timeline:
testdata %>% 
  ggplot(aes(datetime, colour=testdata$Month)) + 
  geom_freqpoly(binwidth = 86400) + # 86400 seconds = 1 day
  xlab("Number of Attacks") +
  ylab("Timelinee") +
  labs(colour="Month")

gif <- animate(
  ggplot(testdata, aes(datetime, time_diff_in_minute)) +
    geom_point(size=2)+
    geom_line(aes(colour=testdata$Month))+
    transition_reveal(testdata$datetime)+
    labs(title = "ph vs time vs batch" , xlab = "Time", ylab = "PH"), duration = 20, fps = 20)
gif  

#plot(df_clean$datetime, df_clean$country)

###########################################################################################################################################
#Plot 1
#MAP - Geolocation of the attacks
# clustering
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



##########################################################################################################################################
#Plot 2
#Top countries with highest numbers of attacks
# count of top bad IPs


# Graph 2
# Top countries with highest number of attacks categorised by MONTH OF ATTACK
#Month are only in seven color
ggplot(testdata) +
  aes(x = country_by_group, fill = Month) +
  geom_bar(position = "dodge") +
  scale_fill_brewer(palette = "Dark2", direction = 1) +
  labs(
    x = "Country",
    y = "Number of attacks",
    title = "Countries With Highest Number of attacks in each month"
  ) +
  hrbrthemes::theme_modern_rc()

ggplot(testdata) +
  aes(x = country_by_group, fill = Month) +
  geom_bar() +
  scale_fill_viridis_d(option = "viridis", direction = -1) +
  labs(
    x = "Countries",
    y = "Number of attacks",
    title = "Top 10 countries and the months of attack"
  ) +
  hrbrthemes::theme_modern_rc()

#with percentage
ggplot(testdata, aes(x=as.factor(country_by_group), fill=as.factor(Month)))+
  geom_bar(aes( y=..count../tapply(..count.., ..x.. ,sum)[..x..]), position="dodge" ) +
  geom_text(aes( y=..count../tapply(..count.., ..x.. ,sum)[..x..], label=scales::percent(..count../tapply(..count.., ..x.. ,sum)[..x..], accuracy = .1, trim = TRUE) ),
            stat="count", position=position_dodge(0.9), vjust=-0.5)+
  scale_fill_brewer(palette = "Dark2", direction = 1) +
  labs(
    x = "Country",
    y = "Number of attacks",
    title = "Countries With Highest Number of attacks  each month"
  ) +
  hrbrthemes::theme_modern_rc()

##########################################################################################################################################
#Plot 3
#attacks per day for each month for top countries

testdata %>%
  filter(country_by_group %in% c("China", "United States", "Japan", "Iran", "Taiwan", "Netherlands", "India", "South Korea", "Vietnam", "Russia")) %>%
  ggplot() +
  aes(x = Day, fill = Month) +
  geom_bar() +
  scale_fill_brewer(palette = "Set1", direction = 1) +
  labs(
    x = "Day",
    y = "Number of attacks",
    title = "Attacks per day for each month"
  ) +
  hrbrthemes::theme_modern_rc()


testdata %>%
  filter(country_by_group %in% c("China", "United States", "Japan", "Iran", "Taiwan", "Netherlands", "India", "South Korea", "Vietnam", "Russia")) %>%
  ggplot() +
  aes(x = Day, fill = Month) +
  geom_bar(position = "dodge") +
  scale_fill_brewer(palette = "Paired", direction = 1) +
  labs(
    x = "Day",
    y = "Number of attacks",
    title = "Attacks per day for each month"
  ) +
  hrbrthemes::theme_modern_rc()

testdata %>%
  group_by(Month, Day) %>% count() %>%
  filter(country_by_group %in% c("China", "United States", "Japan", "Iran", "Taiwan", "Netherlands", "India", "South Korea", "Vietnam", "Russia")) %>%
  ggplot() +
  aes(x = Day, y = n, fill = Month) +
  geom_line() +
  scale_fill_brewer(palette = "Paired", direction = 1) +
  labs(
    x = "Day",
    y = "Number of attacks",
    title = "Attacks per day for each month"
  ) +
  hrbrthemes::theme_modern_rc()

###################################
library(zoo)
# Day
df3 <- testdata %>%
  group_by(Month, Day) %>% count()
  
ggplot(df3, aes(x = Day, y = n, group = factor(Month), color = factor(Month))) + 
  geom_line()+
  geom_point() +
  scale_fill_brewer(palette = "Paired")+
  geom_hline(aes(yintercept = 13500))

#################################################


ggplot(df3, aes(x = Month, y=n, group = factor(Month), color = factor(Month))) + 
  geom_boxplot() +
  geom_jitter()+
  ylim(1000,3500)+
  geom_hline(aes(yintercept = mean(n), col= "red"))

ggplot(df3, aes(month(date, label=TRUE, abbr=TRUE), 
                             value, group=factor(year(date)), colour=factor(year(date)))) +
  geom_line() +
  geom_point() +
  labs(x="Month", colour="Year") +
  theme_classic()

#Attacks per day country wise
ggplot(testdata) +
  aes(x = Day, fill = country_by_group) +
  geom_bar() +
  scale_fill_brewer(palette = "Paired", direction = 1) +
  labs(
    x = "Day",
    y = "Number of attacks",
    title = "Attacks per day country wise",
    fill = "country"
  ) +
  hrbrthemes::theme_modern_rc()

###############################################################################################

#newdf <- 
 # df1 %>%
  #  mutate(country = fct_lump(country, n = 10, other_level = "rest countries")) %>%
   # count(country, sort = TRUE)
#View(newdf)

country_by_ip <- testdata %>%
  group_by(country_by_group) %>%
  summarise(mean_mass = count(srcstr, sort = TRUE))

plot <- 
  data1 %>% 
  ggplot(aes(y = datetime, x = ip_by_group)) + geom_line() #672
plot

# Attacks 24*7

ggplot(testdata) +
  aes(x = Day, fill = testdata$Month) +
  geom_bar(fill = "#112446") +
  labs(
    x = "Day",
    y = "Number of attacks",
    title = "Attacks each day",
  ) +
  #coord_cartesian(xlim = c(1,31))+
  theme(axis.text.x = element_blank()) +
  facet_wrap(vars(Hour), scales = "free")



testdata %>%
  ggplot() +
  aes(x = ip_by_group, fill = Month) +
  geom_bar(position = "dodge") +
  scale_fill_viridis_d(option = "viridis", direction = 1) +
  labs(
    x = "Number of attacks",
    title = "Top 10 bad Ip's and their country of origin"
  ) +
  facet_wrap(vars(country_by_group), scales = "free")+
  hrbrthemes::theme_modern_rc()
  
  ggplot(testdata) +
  aes(x = ip_by_group, fill = Month) +
  geom_bar(position = "dodge") +
  scale_fill_viridis_d(option = "inferno", direction = 1) +
  theme_dark() +
  facet_wrap(vars(country_by_group), scales = "free")
  

# Number of Cyberattacks Per Week March 3rd to September 7th (Weeks 10 to 36)

histo<-ggplotGrob(
  data_subset %>% ggplot(aes(x=reorder(fullIP,count),y=count)) + 
    geom_bar(stat='identity') + coord_flip() + theme_fivethirtyeight() + 
    theme(axis.text=element_text(size=8)) + labs(subtitle='top 10 bad IP addresses'))

###############################################33333
install.packages("forecast")
library(forecast)
attack_ts <- ts(df3$n, frequency = 30, start = 03)
plot(attack_ts)
attack_ets <- ses(attack_ts)
plot(attack_ets)
summary(attack_ets)
