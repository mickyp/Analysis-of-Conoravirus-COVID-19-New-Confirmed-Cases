# 2020.03.19
# Introduction to Data Science (UCLA Extension)
# Final Class Project
# Student: Micky Lee (Wen Chi Lee)
# Instructor: Daniel D. Gutierrez

## Analysis of COVID-19 top 10 confirmed cases countries about the new confirmed

# 1. Data access: download the data set into your R environment.
# reading the data
virus <- read.csv("./covid_19_data.csv")  # the csv and source file is in the same directory

# 2. Use the data repository to get a definition of all the variables in the data set. 
# Perform feature engineering to select variables that support your hypothesis.

class(virus)
# [1] "data.frame" 

dim(virus)
# [1] 6722    8

names(virus)
# [1] "SNo"             "ObservationDate" "Province/State"  "Country/Region"  "Last Update"    
# [6] "Confirmed"       "Deaths"          "Recovered"

str
# 'data.frame':	6722 obs. of  8 variables:
# $ SNo            : int  1 2 3 4 5 6 7 8 9 10 ...
# $ ObservationDate: Factor w/ 57 levels "01/22/2020","01/23/2020",..: 1 1 1 1 1 1 1 1 1 1 ...
# $ Province.State : Factor w/ 277 levels ""," Montreal, QC",..: 8 15 34 71 73 82 83 84 85 90 ...
# $ Country.Region : Factor w/ 179 levels " Azerbaijan",..: 99 99 99 99 99 99 99 99 99 99 ...
# $ Last.Update    : Factor w/ 1637 levels "1/22/2020 17:00",..: 1 1 1 1 1 1 1 1 1 1 ...
# $ Confirmed      : num  1 14 6 1 0 26 2 1 4 1 ...
# $ Deaths         : num  0 0 0 0 0 0 0 0 0 0 ...
# $ Recovered      : num  0 0 0 0 0 0 0 0 0 0 ...
 
# check summary, found there is no NA
summary(virus)
#      SNo         ObservationDate   Province.State        Country.Region              Last.Update  
# Min.   :   1   03/18/2020: 284            :2766   Mainland China:1765   2020-03-11T20:00:00: 197  
# 1st Qu.:1681   03/17/2020: 276   Gansu    :  59   US            :1388   2020-03-16T14:38:45: 105  
# Median :3362   03/16/2020: 272   Hebei    :  59   Australia     : 287   2020-03-15T18:20:18:  87  
# Mean   :3362   03/09/2020: 266   Anhui    :  57   Canada        : 208   2020-02-01T19:43:03:  63  
# 3rd Qu.:5042   03/15/2020: 258   Beijing  :  57   France        :  91   2020-02-01T19:53:03:  63  
# Max.   :6722   03/08/2020: 255   Chongqing:  57   UK            :  68   2020-02-24T23:33:02:  63  
#                (Other)   :5111   (Other)  :3667   (Other)       :2915   (Other)            :6144  
#   Confirmed           Deaths          Recovered      
# Min.   :    0.0   Min.   :   0.00   Min.   :    0.0  
# 1st Qu.:    2.0   1st Qu.:   0.00   1st Qu.:    0.0  
# Median :   13.0   Median :   0.00   Median :    0.0  
# Mean   :  601.2   Mean   :  19.86   Mean   :  226.3  
# 3rd Qu.:  108.0   3rd Qu.:   1.00   3rd Qu.:   11.0  
# Max.   :67800.0   Max.   :3122.00   Max.   :56927.0  

head(virus)
#   SNo ObservationDate Province.State Country.Region     Last.Update Confirmed Deaths Recovered
# 1   1      01/22/2020          Anhui Mainland China 1/22/2020 17:00         1      0         0
# 2   2      01/22/2020        Beijing Mainland China 1/22/2020 17:00        14      0         0
# 3   3      01/22/2020      Chongqing Mainland China 1/22/2020 17:00         6      0         0
# 4   4      01/22/2020         Fujian Mainland China 1/22/2020 17:00         1      0         0
# 5   5      01/22/2020          Gansu Mainland China 1/22/2020 17:00         0      0         0
# 6   6      01/22/2020      Guangdong Mainland China 1/22/2020 17:00        26      0         0

# we don't need the SNo
virus <- virus[,-1]

# 3. Perform any data transformations you feel are necessary to achieve the desired goals.
library(lubridate)
library(dplyr)
library(sqldf)

# for directly accessing the names of date frame 

#observationDate should be date format
str(virus$ObservationDate)
# Factor w/ 57 levels "01/22/2020","01/23/2020",..: 1 1 1 1 1 1 1 1 1 1 ...

virus$ObservationDate <- parse_date_time(virus$ObservationDate, "%m/%d/%y")
str(virus$ObservationDate)
# POSIXct[1:6722], format: "2020-01-22" "2020-01-22" "2020-01-22" "2020-01-22" "2020-01-22" 
# "2020-01-22" "2020-01-22" ...

#Last.Update should be date time format
str(virus$Last.Update)
# Factor w/ 1637 levels "1/22/2020 17:00",..: 1 1 1 1 1 1 1 1 1 1 ...

# there are two formats of Last.update
virus$Last.Update[555:565]
# [1] 2/1/2020 19:43      2/1/2020 19:53      2/1/2020 19:53      2/1/2020 19:53     
# [5] 2/1/2020 19:43      2/1/2020 19:43      2020-02-02T23:43:02 2020-02-02T18:03:05
# [9] 2020-02-02T18:03:05 2020-02-02T18:03:05 2020-02-02T18:03:05
# 1637 Levels: 1/22/2020 17:00 1/23/20 17:00 1/24/20 17:00 1/25/20 17:00 1/26/20 16:00 ... 2020-03-18T19:53:03

# for date format 2/1/2020 19:43
lastUpdate_1_560 <- parse_date_time(virus$Last.Update[1:560], "%m/%d/%y %H:%M")

# for date format 2020-02-02T23:43:02
lastUpdate_561_6722 <- parse_date_time(virus$Last.Update[561:6722], "%y-%m-%d %H:%M:%S")

# combine format converted result
virus$Last.Update <- c(lastUpdate_1_560, lastUpdate_561_6722)

str(virus$Last.Update)
# POSIXct[1:6722], format: "2020-01-22 17:00:00" "2020-01-22 17:00:00" "2020-01-22 17:00:00" 
# "2020-01-22 17:00:00" ...

#change the names
names(virus)[2] <- c("ProvinceState")
names(virus)[3] <- c("CountryRegion")
names(virus)

# remove last update
virus <- virus[, c(-4)]



# 4. Use various EDA and simple statistical analysis techniques to gain a deep understanding for the data.

# filter out the specified countries, my country, Taiwan, and the top 10 confirmed cases countries in the world
v <- subset(virus, CountryRegion == "Mainland China" | 
                   CountryRegion == "Italy" | 
                   CountryRegion == "Spain" | 
                   CountryRegion == "Germany" | 
                   CountryRegion == "Iran" |
                   CountryRegion == "US" | 
                   CountryRegion == "France" | 
                   CountryRegion == "South Korea" | 
                   CountryRegion == "Switzerland" | 
                   CountryRegion == "UK" | 
                   CountryRegion == "Taiwan")


# SQL select to produce a new data.frame with specific country and group by ProvinceState, ObservationDate
groupByProvinceStateDate <- function(data, country) {
  # convert date to string before using sqldf to avoid sqldf change the date type
  data$ObservationDate <- as.character(data$ObservationDate)
  
  newData <- sqldf(sprintf("select ObservationDate, ProvinceState, sum(Confirmed) as Confirmed, 
                  sum(Deaths) as Deaths, sum(Recovered) as Recovered
                  from data 
                  where CountryRegion = '%s' 
                  group by ProvinceState, ObservationDate", country))
  
  # convert string to date again 
  newData$ObservationDate <- as.Date(newData$ObservationDate)
  newData
}

# SQL select to produce a new data.frame only group by ObservationDate ignoring ProvinceState
groupByDate <- function(countryData) {
  sqldf("select ObservationDate, sum(Confirmed) as Confirmed, sum(Deaths) as Deaths, 
        sum(Recovered) as Recovered
        from countryData 
        group by ObservationDate")
}

# calculate confirmedNew, DeathsNew, RecoveredNew increasing by date
calculateNew <- function(countryData) {
  countryData %>% 
    mutate(ConfirmedNew = order_by(ObservationDate, Confirmed - lag(Confirmed))) %>%
    mutate(ConfirmedNew = ifelse(is.na(ConfirmedNew), 0, ConfirmedNew)) %>%
    mutate(DeathsNew = order_by(ObservationDate, Deaths - lag(Deaths))) %>%
    mutate(DeathsNew = ifelse(is.na(DeathsNew), 0, DeathsNew)) %>%
    mutate(RecoveredNew = order_by(ObservationDate, Recovered - lag(Recovered))) %>%
    mutate(RecoveredNew = ifelse(is.na(RecoveredNew), 0, RecoveredNew))
}


##################
### CHINA DATA ###
##################

# calculate China by province and date
chinaByProvince <- groupByProvinceStateDate(v, "Mainland China")
head(chinaByProvince)
#   ObservationDate ProvinceState Confirmed Deaths Recovered
# 1      2020-01-22         Anhui         1      0         0
# 2      2020-01-23         Anhui         9      0         0
# 3      2020-01-24         Anhui        15      0         0
# 4      2020-01-25         Anhui        39      0         0
# 5      2020-01-26         Anhui        60      0         0
# 6      2020-01-27         Anhui        70      0         0

# calculate china confirmed by date ignoring province
chinaByDate <- groupByDate(chinaByProvince)
head(chinaByDate)
#   ObservationDate Confirmed Deaths Recovered
# 1      2020-01-22       547     17        28
# 2      2020-01-23       639     18        30
# 3      2020-01-24       916     26        36
# 4      2020-01-25      1399     42        39
# 5      2020-01-26      2062     56        49
# 6      2020-01-27      2863     82        58

# calculate China's confirmedNew, DeathsNew, RecoveredNew by date
chinaNew <- calculateNew(chinaByDate)
head(chinaNew)
#   ObservationDate Confirmed Deaths Recovered ConfirmedNew DeathsNew RecoveredNew
# 1      2020-01-22       547     17        28            0         0            0
# 2      2020-01-23       639     18        30           92         1            2
# 3      2020-01-24       916     26        36          277         8            6
# 4      2020-01-25      1399     42        39          483        16            3
# 5      2020-01-26      2062     56        49          663        14           10
# 6      2020-01-27      2863     82        58          801        26            9

##################
### ITALY DATA ###
##################

# Italy doesn't have province or state data, so just calculate the delta New
# calculate IranconfirmedNew, DeathsNew, RecoveredNew by date
italy <- subset(v, CountryRegion == "Italy")
italyNew <- calculateNew(italy)
italyNew <- italyNew[, c(-2,-3)]    # remove column ProvinceState, CountryRegion
italyNew$ObservationDate <- as.Date(italyNew$ObservationDate) #convert to date
head(italyNew)
#   ObservationDate Confirmed Deaths Recovered ConfirmedNew DeathsNew RecoveredNew
# 1      2020-01-31         2      0         0            0         0            0
# 2      2020-02-01         2      0         0            0         0            0
# 3      2020-02-02         2      0         0            0         0            0
# 4      2020-02-03         2      0         0            0         0            0
# 5      2020-02-04         2      0         0            0         0            0
# 6      2020-02-05         2      0         0            0         0            0

##################
### SPAIN DATA ###
##################

# Spain doesn't have province or state data, so just calculate the delta New
# calculate IranconfirmedNew, DeathsNew, RecoveredNew by date
spain <- subset(v, CountryRegion == "Spain")
spainNew <- calculateNew(spain)
spainNew <- spainNew[, c(-2,-3)]    # remove column ProvinceState, CountryRegion,
spainNew$ObservationDate <- as.Date(spainNew$ObservationDate) #convert to date
head(spainNew)
#   ObservationDate Confirmed Deaths Recovered ConfirmedNew DeathsNew RecoveredNew
# 1      2020-02-01         1      0         0            0         0            0
# 2      2020-02-02         1      0         0            0         0            0
# 3      2020-02-03         1      0         0            0         0            0
# 4      2020-02-04         1      0         0            0         0            0
# 5      2020-02-05         1      0         0            0         0            0
# 6      2020-02-06         1      0         0            0         0            0


####################
### Germany DATA ###
####################

# calculate Germany by province and date
germanyByProvince <- groupByProvinceStateDate(v, "Germany")
head(germanyByProvince)
#   ObservationDate ProvinceState Confirmed Deaths Recovered
# 1      2020-02-01                       8      0         0
# 2      2020-02-02                      10      0         0
# 3      2020-02-03                      12      0         0
# 4      2020-02-04                      12      0         0
# 5      2020-02-05                      12      0         0
# 6      2020-02-06                      12      0         0

# calculate Germany confirmed by date ignoring province
germanyByDate <- groupByDate(germanyByProvince)
head(germanyByDate)
#   ObservationDate Confirmed Deaths Recovered
# 1      2020-01-28         4      0         0
# 2      2020-01-29         4      0         0
# 3      2020-01-30         4      0         0
# 4      2020-01-31         5      0         0
# 5      2020-02-01         8      0         0
# 6      2020-02-02        10      0         0

# calculate Germany confirmedNew, DeathsNew, RecoveredNew by date
germanyNew <- calculateNew(germanyByDate)
head(germanyNew)
#   ObservationDate Confirmed Deaths Recovered ConfirmedNew DeathsNew RecoveredNew
# 1      2020-01-28         4      0         0            0         0            0
# 2      2020-01-29         4      0         0            0         0            0
# 3      2020-01-30         4      0         0            0         0            0
# 4      2020-01-31         5      0         0            1         0            0
# 5      2020-02-01         8      0         0            3         0            0
# 6      2020-02-02        10      0         0            2         0            0

#################
### IRAN DATA ###
#################

# Iran doesn't have province or state data, so just calculate the delta New
# calculate IranconfirmedNew, DeathsNew, RecoveredNew by date
iran <- subset(v, CountryRegion == "Iran")
iranNew <- calculateNew(iran)
iranNew <- iranNew[, c(-2,-3)]    # remove column ProvinceState, CountryRegion
iranNew$ObservationDate <- as.Date(iranNew$ObservationDate) #convert to date
head(iranNew)
#   ObservationDate Confirmed Deaths Recovered ConfirmedNew DeathsNew RecoveredNew
# 1      2020-02-19         2      2         0            0         0            0
# 2      2020-02-20         5      2         0            3         0            0
# 3      2020-02-21        18      4         0           13         2            0
# 4      2020-02-22        28      5         0           10         1            0
# 5      2020-02-23        43      8         0           15         3            0
# 6      2020-02-24        61     12         0           18         4            0

##############################
### THE UNITED STATES DATA ###
##############################

# calculate the United States by state and date
usByState <- groupByProvinceStateDate(v, "US")
head(usByState)
#   ObservationDate       ProvinceState Confirmed Deaths Recovered
# 1      2020-03-03  Norfolk County, MA         1      0         0
# 2      2020-03-04  Norfolk County, MA         1      0         0
# 3      2020-03-05  Norfolk County, MA         1      0         0
# 4      2020-03-06  Norfolk County, MA         2      0         0
# 5      2020-03-07  Norfolk County, MA         2      0         0
# 6      2020-03-13             Alabama         5      0         0

# calculate the United States confirmed by date ignoring state
usByDate <- groupByDate(usByState)
head(usByDate)
#   ObservationDate Confirmed Deaths Recovered
# 1      2020-01-22         1      0         0
# 2      2020-01-23         1      0         0
# 3      2020-01-24         2      0         0
# 4      2020-01-25         2      0         0
# 5      2020-01-26         5      0         0
# 6      2020-01-27         5      0         0

# calculate the United States' confirmedNew, DeathsNew, RecoveredNew by date
usNew <- calculateNew(usByDate)
head(usNew)
#   ObservationDate Confirmed Deaths Recovered ConfirmedNew DeathsNew RecoveredNew
# 1      2020-01-22         1      0         0            0         0            0
# 2      2020-01-23         1      0         0            0         0            0
# 3      2020-01-24         2      0         0            1         0            0
# 4      2020-01-25         2      0         0            0         0            0
# 5      2020-01-26         5      0         0            3         0            0
# 6      2020-01-27         5      0         0            0         0            0

###################
### France DATA ###
###################

# calculate France by state and date
franceByProvince <- groupByProvinceStateDate(v, "France")
tail(franceByProvince)
#    ObservationDate ProvinceState Confirmed Deaths Recovered
# 86      2020-03-13     St Martin         2      0         0
# 87      2020-03-14     St Martin         2      0         0
# 88      2020-03-15     St Martin         2      0         0
# 89      2020-03-16     St Martin         2      0         0
# 90      2020-03-17     St Martin         2      0         0
# 91      2020-03-18     St Martin         3      0         0

# calculate France confirmed by date ignoring state
franceByDate <- groupByDate(franceByProvince)
head(franceByDate)
#   ObservationDate Confirmed Deaths Recovered
# 1      2020-01-24         2      0         0
# 2      2020-01-25         3      0         0
# 3      2020-01-26         3      0         0
# 4      2020-01-27         3      0         0
# 5      2020-01-28         4      0         0
# 6      2020-01-29         5      0         0

# calculate the United States' confirmedNew, DeathsNew, RecoveredNew by date
franceNew <- calculateNew(franceByDate)
head(franceNew)
#   ObservationDate Confirmed Deaths Recovered ConfirmedNew DeathsNew RecoveredNew
# 1      2020-01-24         2      0         0            0         0            0
# 2      2020-01-25         3      0         0            1         0            0
# 3      2020-01-26         3      0         0            0         0            0
# 4      2020-01-27         3      0         0            0         0            0
# 5      2020-01-28         4      0         0            1         0            0
# 6      2020-01-29         5      0         0            1         0            0

########################
### South Korea DATA ###
########################

# South Korea doesn't have province or state data, so just calculate the delta New
# calculate IranconfirmedNew, DeathsNew, RecoveredNew by date
korea <- subset(v, CountryRegion == "South Korea")
koreaNew <- calculateNew(korea)
koreaNew <- koreaNew[, c(-2,-3)]    # remove column ProvinceState, CountryRegion
koreaNew$ObservationDate <- as.Date(koreaNew$ObservationDate) #convert to date
head(koreaNew)
#   ObservationDate Confirmed Deaths Recovered ConfirmedNew DeathsNew RecoveredNew
# 1      2020-01-22         1      0         0            0         0            0
# 2      2020-01-23         1      0         0            0         0            0
# 3      2020-01-24         2      0         0            1         0            0
# 4      2020-01-25         2      0         0            0         0            0
# 5      2020-01-26         3      0         0            1         0            0
# 6      2020-01-27         4      0         0            1         0            0

########################
### Switzerland DATA ###
########################

# Switzerland doesn't have province or state data, so just calculate the delta New
# calculate IranconfirmedNew, DeathsNew, RecoveredNew by date
switzerland <- subset(v, CountryRegion == "Switzerland")
switzerlandNew <- calculateNew(switzerland)
switzerlandNew <- switzerlandNew[, c(-2,-3)]    # remove column ProvinceState, CountryRegion
switzerlandNew$ObservationDate <- as.Date(switzerlandNew$ObservationDate) #convert to date
head(switzerlandNew)
#   ObservationDate Confirmed Deaths Recovered ConfirmedNew DeathsNew RecoveredNew
# 1      2020-02-25         1      0         0            0         0            0
# 2      2020-02-26         1      0         0            0         0            0
# 3      2020-02-27         8      0         0            7         0            0
# 4      2020-02-28         8      0         0            0         0            0
# 5      2020-02-29        18      0         0           10         0            0
# 6      2020-03-01        27      0         0            9         0            0

##############################
### THE UNITED KINGDOM DATA ###
##############################

# calculate UK by province and date
ukByProvince <- groupByProvinceStateDate(v, "UK")
tail(ukByProvince)
#    ObservationDate  ProvinceState Confirmed Deaths Recovered
# 63      2020-03-13 United Kingdom       798      8        18
# 64      2020-03-14 United Kingdom      1140     21        18
# 65      2020-03-15 United Kingdom      1140     21        18
# 66      2020-03-16 United Kingdom      1543     55        20
# 67      2020-03-17 United Kingdom      1950     55        52
# 68      2020-03-18 United Kingdom      2626     71        65

# calculate UK confirmed by date ignoring state
ukByDate <- groupByDate(ukByProvince)
head(ukByDate)
#   ObservationDate Confirmed Deaths Recovered
# 1      2020-01-31         2      0         0
# 2      2020-02-01         2      0         0
# 3      2020-02-02         2      0         0
# 4      2020-02-03         2      0         0
# 5      2020-02-04         2      0         0
# 6      2020-02-05         2      0         0

# calculate UK confirmedNew, DeathsNew, RecoveredNew by date
ukNew <- calculateNew(ukByDate)
head(ukNew)
#   ObservationDate Confirmed Deaths Recovered ConfirmedNew DeathsNew RecoveredNew
# 1      2020-01-31         2      0         0            0         0            0
# 2      2020-02-01         2      0         0            0         0            0
# 3      2020-02-02         2      0         0            0         0            0
# 4      2020-02-03         2      0         0            0         0            0
# 5      2020-02-04         2      0         0            0         0            0
# 6      2020-02-05         2      0         0            0         0            0

###################
### TAIWAN DATA ###
###################

# Taiwan doesn't have province or state data, so just calculate the delta New
# calculate IranconfirmedNew, DeathsNew, RecoveredNew by date
taiwan <- subset(v, CountryRegion == "Taiwan")
taiwanNew <- calculateNew(taiwan)
taiwanNew <- taiwanNew[, c(-2,-3)]    # remove column ProvinceState, CountryRegion
taiwanNew$ObservationDate <- as.Date(taiwanNew$ObservationDate) #convert to date
head(taiwanNew)
#   ObservationDate Confirmed Deaths Recovered ConfirmedNew DeathsNew RecoveredNew
# 1      2020-01-22         1      0         0            0         0            0
# 2      2020-01-23         1      0         0            0         0            0
# 3      2020-01-24         3      0         0            2         0            0
# 4      2020-01-25         3      0         0            0         0            0
# 5      2020-01-26         4      0         0            1         0            0
# 6      2020-01-27         5      0         0            1         0            0


# 5. Use R’s plotting features to produce both exploratory and expository data visualizations.

library(ggplot2)
# install.packages("GGally")
library(GGally)

# Sys.setlocale("LC_ALL","English")
# increase or not increase by time
# par(mfrow = c(4, 2))

# draw the plot according to the Confirmed New count by date
ggplot(data=chinaNew, mapping=aes(x=ObservationDate, y=ConfirmedNew, color=ObservationDate)) + 
  geom_point() + ggtitle("China") + xlab("Observation Date") + ylab("Confirmed New") +
  theme(legend.position = "none")

ggplot(data=italyNew, mapping=aes(x=ObservationDate, y=ConfirmedNew, color=ObservationDate)) + 
  geom_point() + ggtitle("Italy") + xlab("Observation Date") + ylab("Confirmed New") +
  theme(legend.position = "none")

ggplot(data=spainNew, mapping=aes(x=ObservationDate, y=ConfirmedNew, color=ObservationDate)) + 
  geom_point() + ggtitle("Spain") + xlab("Observation Date") + ylab("Confirmed New") +
  theme(legend.position = "none")

ggplot(data=germanyNew, mapping=aes(x=ObservationDate, y=ConfirmedNew, color=ObservationDate)) + 
  geom_point() + ggtitle("Germany") + xlab("Observation Date") + ylab("Confirmed New") +
  theme(legend.position = "none")

ggplot(data=iranNew, mapping=aes(x=ObservationDate, y=ConfirmedNew, color=ObservationDate)) + 
  geom_point() + ggtitle("Iran") + xlab("Observation Date") + ylab("Confirmed New") +
  theme(legend.position = "none")

ggplot(data=usNew, mapping=aes(x=ObservationDate, y=ConfirmedNew, color=ObservationDate)) + 
  geom_point() + ggtitle("US") + xlab("Observation Date") + ylab("Confirmed New") +
  theme(legend.position = "none")

ggplot(data=franceNew, mapping=aes(x=ObservationDate, y=ConfirmedNew, color=ObservationDate)) + 
  geom_point() + ggtitle("France") + xlab("Observation Date") + ylab("Confirmed New") +
  theme(legend.position = "none")

ggplot(data=koreaNew, mapping=aes(x=ObservationDate, y=ConfirmedNew, color=ObservationDate)) + 
  geom_point() + ggtitle("South Korea") + xlab("Observation Date") + ylab("Confirmed New") +
  theme(legend.position = "none")

ggplot(data=switzerlandNew, mapping=aes(x=ObservationDate, y=ConfirmedNew, color=ObservationDate)) + 
  geom_point() + ggtitle("Switzerland") + xlab("Observation Date") + ylab("Confirmed New") +
  theme(legend.position = "none")

ggplot(data=ukNew, mapping=aes(x=ObservationDate, y=ConfirmedNew, color=ObservationDate)) + 
  geom_point() + ggtitle("UK") + xlab("Observation Date") + ylab("Confirmed New") +
  theme(legend.position = "none")

ggplot(data=taiwanNew, mapping=aes(x=ObservationDate, y=ConfirmedNew, color=ObservationDate)) + 
  geom_point() + ggtitle("Taiwan") + xlab("Observation Date") + ylab("Confirmed New") +
  theme(legend.position = "none")

# According to above scatter plots show that the China is the only abnormal one. 
# When every country is uncontrolled increasing "Confirmed New" day by day, 
# only China always keeps the new confirmed cases in double digits.
# Needless to say, China once reported 15133 new confirmed cases. 
# It's impossible to lower to the lowerest daily new confirmed country in the world.
# With the news reported, it's only because the Xi, the president of China, 
# said "no more bad news, only give me good news." From then on, 
# the reported new confirmed cases are dramstically down, which is obviously China makes the fake report data.

# Boxplots
ggplot(chinaNew)+geom_boxplot(aes(y=ConfirmedNew,color=1)) + 
  ggtitle("China") + theme(legend.position = "none")

ggplot(italyNew)+geom_boxplot(aes(y=ConfirmedNew,color=1)) + 
  ggtitle("Italy") + theme(legend.position = "none")

ggplot(spainNew)+geom_boxplot(aes(y=ConfirmedNew,color=1)) + 
  ggtitle("Spain") + theme(legend.position = "none")

ggplot(germanyNew)+geom_boxplot(aes(y=ConfirmedNew,color=1)) + 
  ggtitle("Germany") + theme(legend.position = "none")

ggplot(iranNew)+geom_boxplot(aes(y=ConfirmedNew,color=1)) + 
  ggtitle("Iran") + theme(legend.position = "none")

ggplot(usNew)+geom_boxplot(aes(y=ConfirmedNew,color=1)) + 
  ggtitle("US") + theme(legend.position = "none")

ggplot(franceNew)+geom_boxplot(aes(y=ConfirmedNew,color=1)) + 
  ggtitle("France") + theme(legend.position = "none")

ggplot(koreaNew)+geom_boxplot(aes(y=ConfirmedNew,color=1)) + 
  ggtitle("South Korea") + theme(legend.position = "none")

ggplot(switzerlandNew)+geom_boxplot(aes(y=ConfirmedNew,color=1)) + 
  ggtitle("Switzerland") + theme(legend.position = "none")

ggplot(ukNew)+geom_boxplot(aes(y=ConfirmedNew,color=1)) + 
  ggtitle("UK") + theme(legend.position = "none")

ggplot(taiwanNew)+geom_boxplot(aes(y=ConfirmedNew,color=1)) + 
  ggtitle("Taiwan") + theme(legend.position = "none")

# Boxplots seem not so useful except iran data

# According to above boxplots, only Iran has a normal boxplot because it does not going too higher 
# at just one time.

# compute the correlation matrix

## CHINA
cor(chinaNew[,c(2:7)])
#                Confirmed     Deaths  Recovered ConfirmedNew   DeathsNew RecoveredNew
# Confirmed     1.00000000  0.9620572  0.7870693   -0.2264827  0.09757346    0.7861816
# Deaths        0.96205718  1.0000000  0.9170125   -0.3726086 -0.11122551    0.7454746
# Recovered     0.78706933  0.9170125  1.0000000   -0.4527927 -0.36514645    0.5230210
# ConfirmedNew -0.22648265 -0.3726086 -0.4527927    1.0000000  0.69482854   -0.1936115
# DeathsNew     0.09757346 -0.1112255 -0.3651465    0.6948285  1.00000000    0.2509042
# RecoveredNew  0.78618158  0.7454746  0.5230210   -0.1936115  0.25090417    1.0000000

ggpairs(chinaNew[,c(2:7)]) + ggtitle("China Pairs")
# the scatterplots shows the same trend as correlations

## ITALY
cor(italyNew[,c(2:7)])
#              Confirmed    Deaths Recovered ConfirmedNew DeathsNew RecoveredNew
# Confirmed    1.0000000 0.9929454 0.9925182    0.9122029 0.9349864    0.8498315
# Deaths       0.9929454 1.0000000 0.9945081    0.8888941 0.9318168    0.8593282
# Recovered    0.9925182 0.9945081 1.0000000    0.8889261 0.9227915    0.8852015
# ConfirmedNew 0.9122029 0.8888941 0.8889261    1.0000000 0.9613616    0.8412318
# DeathsNew    0.9349864 0.9318168 0.9227915    0.9613616 1.0000000    0.8451573
# RecoveredNew 0.8498315 0.8593282 0.8852015    0.8412318 0.8451573    1.0000000

ggpairs(italyNew[,c(2:7)]) + ggtitle("Italy Pairs")
# the scatterplots shows the same trend as correlations

## Spain
cor(spainNew[,c(2:7)])
#              Confirmed    Deaths Recovered ConfirmedNew DeathsNew RecoveredNew
# Confirmed    1.0000000 0.9851107 0.9772678    0.8857230 0.8967061    0.5874379
# Deaths       0.9851107 1.0000000 0.9892781    0.8220942 0.8967246    0.5971242
# Recovered    0.9772678 0.9892781 1.0000000    0.7930421 0.9088898    0.6828530
# ConfirmedNew 0.8857230 0.8220942 0.7930421    1.0000000 0.8264402    0.4419344
# DeathsNew    0.8967061 0.8967246 0.9088898    0.8264402 1.0000000    0.7644993
# RecoveredNew 0.5874379 0.5971242 0.6828530    0.4419344 0.7644993    1.0000000

ggpairs(spainNew[,c(2:7)]) + ggtitle("Spain Pairs")
# the scatterplots shows the same trend as correlations

## Germany
cor(germanyNew[,c(2:7)])
#              Confirmed    Deaths Recovered ConfirmedNew DeathsNew RecoveredNew
# Confirmed    1.0000000 0.9905134 0.9462981    0.9760822 0.8824248    0.7087381
# Deaths       0.9905134 1.0000000 0.9219477    0.9638850 0.9019148    0.6937567
# Recovered    0.9462981 0.9219477 1.0000000    0.9394256 0.8311824    0.7427014
# ConfirmedNew 0.9760822 0.9638850 0.9394256    1.0000000 0.8744653    0.7870116
# DeathsNew    0.8824248 0.9019148 0.8311824    0.8744653 1.0000000    0.6078585
# RecoveredNew 0.7087381 0.6937567 0.7427014    0.7870116 0.6078585    1.0000000

ggpairs(germanyNew[,c(2:7)]) + ggtitle("Germany Pairs")
# the scatterplots shows the same trend as correlations

## IRAN
cor(iranNew[,c(2:7)])
#              Confirmed    Deaths Recovered ConfirmedNew DeathsNew RecoveredNew
# Confirmed    1.0000000 0.9659078 0.9877551    0.8718847 0.9832549    0.4292303
# Deaths       0.9659078 1.0000000 0.9640209    0.7583867 0.9889921    0.3623454
# Recovered    0.9877551 0.9640209 1.0000000    0.8139471 0.9806548    0.4829727
# ConfirmedNew 0.8718847 0.7583867 0.8139471    1.0000000 0.7888733    0.4290633
# DeathsNew    0.9832549 0.9889921 0.9806548    0.7888733 1.0000000    0.3859530
# RecoveredNew 0.4292303 0.3623454 0.4829727    0.4290633 0.3859530    1.0000000

ggpairs(iranNew[,c(2:7)]) + ggtitle("Iran Pairs")
# the scatterplots shows the same trend as correlations

## US
cor(usNew[,c(2:7)])
#              Confirmed    Deaths Recovered ConfirmedNew DeathsNew RecoveredNew
# Confirmed    1.0000000 0.9826370 0.7835495    0.9772639 0.8635338    0.6471476
# Deaths       0.9826370 1.0000000 0.7305503    0.9741984 0.9031358    0.5632907
# Recovered    0.7835495 0.7305503 1.0000000    0.6548981 0.4520740    0.9579022
# ConfirmedNew 0.9772639 0.9741984 0.6548981    1.0000000 0.9288341    0.4941703
# DeathsNew    0.8635338 0.9031358 0.4520740    0.9288341 1.0000000    0.2570033
# RecoveredNew 0.6471476 0.5632907 0.9579022    0.4941703 0.2570033    1.0000000

ggpairs(usNew[,c(2:7)]) + ggtitle("US Pairs")
# the scatterplots shows the same trend as correlations

## France
cor(franceNew[,c(2:7)])
#                Confirmed      Deaths Recovered ConfirmedNew   DeathsNew RecoveredNew
# Confirmed     1.00000000  0.99158430 0.5116006   0.85640981  0.50067340  -0.09373728
# Deaths        0.99158430  1.00000000 0.5124951   0.87909969  0.57759113  -0.09144874
# Recovered     0.51160063  0.51249511 1.0000000   0.46440169  0.34615095   0.10779886
# ConfirmedNew  0.85640981  0.87909969 0.4644017   1.00000000  0.82397892  -0.08396031
# DeathsNew     0.50067340  0.57759113 0.3461510   0.82397892  1.00000000  -0.06093633
# RecoveredNew -0.09373728 -0.09144874 0.1077989  -0.08396031 -0.06093633   1.00000000

ggpairs(franceNew[,c(2:7)]) + ggtitle("France Pairs")
# the scatterplots shows the same trend as correlations

## South Korea
cor(koreaNew[,c(2:7)])
#              Confirmed    Deaths   Recovered ConfirmedNew DeathsNew RecoveredNew
# Confirmed    1.0000000 0.9758820  0.67507765   0.41782904 0.6196650   0.48556427
# Deaths       0.9758820 1.0000000  0.79495465   0.27245605 0.5927068   0.55849508
# Recovered    0.6750777 0.7949546  1.00000000  -0.03504501 0.3100229   0.73699724
# ConfirmedNew 0.4178290 0.2724560 -0.03504501   1.00000000 0.4291818  -0.04419737
# DeathsNew    0.6196650 0.5927068  0.31002291   0.42918182 1.0000000   0.08050110
# RecoveredNew 0.4855643 0.5584951  0.73699724  -0.04419737 0.0805011   1.00000000

ggpairs(koreaNew[,c(2:7)]) + ggtitle("South Korea Pairs")
# the scatterplots shows the same trend as correlations

## Switzerland
cor(switzerlandNew[,c(2:7)])
#              Confirmed    Deaths Recovered ConfirmedNew   DeathsNew RecoveredNew
# Confirmed    1.0000000 0.9757291 0.7533963    0.7139401  0.53690650   0.49440750
# Deaths       0.9757291 1.0000000 0.7573855    0.6927869  0.64174351   0.53766168
# Recovered    0.7533963 0.7573855 1.0000000    0.4342335  0.19437474   0.83828165
# ConfirmedNew 0.7139401 0.6927869 0.4342335    1.0000000  0.58532231   0.17039648
# DeathsNew    0.5369065 0.6417435 0.1943747    0.5853223  1.00000000  -0.04152744
# RecoveredNew 0.4944075 0.5376617 0.8382817    0.1703965 -0.04152744   1.00000000

ggpairs(switzerlandNew[,c(2:7)]) + ggtitle("Switzerland Pairs")
# the scatterplots shows the same trend as correlations

## UK
cor(ukNew[,c(2:7)])
#              Confirmed    Deaths Recovered ConfirmedNew DeathsNew RecoveredNew
# Confirmed    1.0000000 0.9701856 0.9095921    0.9300630 0.6432759    0.6543519
# Deaths       0.9701856 1.0000000 0.8645728    0.9112083 0.7272639    0.6802265
# Recovered    0.9095921 0.8645728 1.0000000    0.8436678 0.4316722    0.7340866
# ConfirmedNew 0.9300630 0.9112083 0.8436678    1.0000000 0.6944427    0.5937232
# DeathsNew    0.6432759 0.7272639 0.4316722    0.6944427 1.0000000    0.1385369
# RecoveredNew 0.6543519 0.6802265 0.7340866    0.5937232 0.1385369    1.0000000

ggpairs(ukNew[,c(2:7)]) + ggtitle("UK Pairs")
# the scatterplots shows the same trend as correlations

## TAIWAN
cor(taiwanNew[,c(2:7)])
#                Confirmed    Deaths   Recovered ConfirmedNew   DeathsNew RecoveredNew
# Confirmed     1.00000000 0.7521506  0.93043655   0.66884939 -0.05469745   0.30874326
# Deaths        0.75215056 1.0000000  0.68952765   0.27063928  0.11811390   0.30953920
# Recovered     0.93043655 0.6895276  1.00000000   0.50585341 -0.07625291   0.36783461
# ConfirmedNew  0.66884939 0.2706393  0.50585341   1.00000000  0.01018845   0.11513149
# DeathsNew    -0.05469745 0.1181139 -0.07625291   0.01018845  1.00000000  -0.05903408
# RecoveredNew  0.30874326 0.3095392  0.36783461   0.11513149 -0.05903408   1.00000000

ggpairs(taiwanNew[,c(2:7)]) + ggtitle("Taiwan Pairs")
# the scatterplots shows the same trend as correlations


# 6. Select one or more of R’s statistical learning algorithms to make predictions, 
# and/or discoveries.

# add country back
chinaNew$Country <- c("China")
italyNew$Country <- c("Italy")
spainNew$Country <- c("Spain")
germanyNew$Country <- c("Germany")
iranNew$Country <- c("Iran")
usNew$Country <- c("US")
franceNew$Country <- c("France")
koreaNew$Country <- c("South Korea")
switzerlandNew$Country <- c("Switzerland")
ukNew$Country <- c("UK")
taiwanNew$Country <- c("Taiwan")

# try to eliminate the date, just focusing on the day sequence occurred
addSeq <- function(data) {
  n <- nrow(data)
  data$seq <- c(1:n)
  data
}

# add sequence
chinaNew <- addSeq(chinaNew)
italyNew <- addSeq(italyNew)
spainNew <- addSeq(spainNew)
germanyNew <- addSeq(germanyNew)
iranNew <- addSeq(iranNew)
usNew <- addSeq(usNew)
franceNew <- addSeq(franceNew)
koreaNew <- addSeq(koreaNew)
switzerlandNew <- addSeq(switzerlandNew)
ukNew <- addSeq(ukNew)
taiwanNew <- addSeq(taiwanNew)

# I choose Iran as the training data set to do the linear regression

summary(iranNew)
# ObservationDate        Confirmed         Deaths         Recovered     ConfirmedNew   
# Min.   :2020-02-19   Min.   :    2   Min.   :   2.0   Min.   :   0   Min.   :   0.0  
# 1st Qu.:2020-02-26   1st Qu.:  139   1st Qu.:  19.0   1st Qu.:  49   1st Qu.:  44.0  
# Median :2020-03-04   Median : 2922   Median :  92.0   Median : 552   Median : 591.0  
# Mean   :2020-03-04   Mean   : 5201   Mean   : 247.1   Mean   :1516   Mean   : 598.6  
# 3rd Qu.:2020-03-11   3rd Qu.: 9000   3rd Qu.: 354.0   3rd Qu.:2959   3rd Qu.:1075.0  
# Max.   :2020-03-18   Max.   :17361   Max.   :1135.0   Max.   :5389   Max.   :1365.0  
#   DeathsNew       RecoveredNew      Country               seq    
# Min.   :  0.00   Min.   :   0.0   Length:29          Min.   : 1  
# 1st Qu.:  4.00   1st Qu.:   0.0   Class :character   1st Qu.: 8  
# Median : 15.00   Median :  24.0   Mode  :character   Median :15  
# Mean   : 39.07   Mean   : 185.8                      Mean   :15  
# 3rd Qu.: 63.00   3rd Qu.: 228.0                      3rd Qu.:22  
# Max.   :147.00   Max.   :1631.0                      Max.   :29 

head(iranNew)
#   ObservationDate Confirmed Deaths Recovered ConfirmedNew DeathsNew RecoveredNew Country seq
# 1      2020-02-19         2      2         0            0         0            0    Iran   1
# 2      2020-02-20         5      2         0            3         0            0    Iran   2
# 3      2020-02-21        18      4         0           13         2            0    Iran   3
# 4      2020-02-22        28      5         0           10         1            0    Iran   4
# 5      2020-02-23        43      8         0           15         3            0    Iran   5
# 6      2020-02-24        61     12         0           18         4            0    Iran   6

n <- nrow(iranNew)
ntrain <- round(n * 0.6)
set.seed(206) # set seed for reproductible results
tindex <- sample(n, ntrain)
trainIran <- iranNew[tindex,] # training set
testIran <- iranNew[-tindex,] # test set

formula <- ConfirmedNew ~ ObservationDate + Confirmed + Deaths + 
  Recovered + DeathsNew + RecoveredNew

lm1 <- lm(formula, data=trainIran)
summary(lm1)

# Call:
# lm(formula = ConfirmedNew ~ ., data = trainIran)
# 
# Residuals:
#      Min       1Q   Median       3Q      Max 
# -209.496  -63.841    1.933   78.064  225.178 
# 
# Coefficients:
#                   Estimate Std. Error t value Pr(>|t|)  
# (Intercept)     -5.199e+05  2.548e+05  -2.041   0.0686 .
# ObservationDate  3.286e-04  1.610e-04   2.041   0.0685 .
# Confirmed        2.048e-01  7.116e-02   2.878   0.0164 *
# Deaths          -1.497e-01  6.507e-01  -0.230   0.8227  
# Recovered       -3.711e-01  1.349e-01  -2.750   0.0205 *
# DeathsNew       -6.418e+00  9.032e+00  -0.711   0.4936  
# RecoveredNew     1.761e-01  2.851e-01   0.618   0.5507  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 134.3 on 10 degrees of freedom
# Multiple R-squared:  0.9467,	Adjusted R-squared:  0.9148 
# F-statistic: 29.62 on 6 and 10 DF,  p-value: 8.229e-06


# Only Confirmed and Recovered p-values are close to 0 with 1 signif. code, and the R-squared is 94.67%, 
# which means there are relationship between the predictor the variables.

par(mfrow=c(2,2))
plot(lm1)
# The Residuals-vs-Fitted plot shows there is a somewhat linear pattern.
# The Normal Q-Q plot shows residuals are normally distributed
# The Scale-Location plot shows the residuals are spread almost equally along 
# the ranges of predictors.
# The Residuals-vs-Leverage shows there are some outliers and influential observation seems exists.


predictIran <- predict(lm1, newdata = testIran)
cor(predictIran, testIran$ConfirmedNew)
# [1] 0.9846674
# It shows 98.46%, which means the prericted and the model are positively linearly related.

# 7. In the case of predictions, use the trained algorithm on new data and make a 
# case for the algorithm’s accuracy.

# TRY to predict with other country

predictChina <- predict(lm1, newdata = chinaNew)
cor(predictChina, chinaNew$ConfirmedNew)
# [1] 0.4457583
# the model is not suited for China

predictItaly <- predict(lm1, newdata = italyNew)
cor(predictItaly, italyNew$ConfirmedNew)
# [1] 0.7914265
# the model is suited for Italy

predictSpain <- predict(lm1, newdata = spainNew)
cor(predictSpain, spainNew$ConfirmedNew)
# [1] 0.8147282
# the model is suited for Spain

predictGermany <- predict(lm1, newdata = germanyNew)
cor(predictGermany, germanyNew$ConfirmedNew)
# [1] 0.8927174
# the model is suited for Germany

predictUs <- predict(lm1, newdata = usNew)
cor(predictUs, usNew$ConfirmedNew)
# [1] 0.8180316
# the model is suited for US

predictFrance <- predict(lm1, newdata = franceNew)
cor(predictFrance, franceNew$ConfirmedNew)
# [1] 0.7410682
# the model is suited for France

predictKorea <- predict(lm1, newdata = koreaNew)
cor(predictKorea, koreaNew$ConfirmedNew)
# [1] 0.5055457
# the model is not suited for South Korea

predictSwitzerland <- predict(lm1, newdata = switzerlandNew)
cor(predictSwitzerland, switzerlandNew$ConfirmedNew)
# [1] 0.6885858
# the model is suited for Switzerland

predictUK <- predict(lm1, newdata = ukNew)
cor(predictUK, ukNew$ConfirmedNew)
# [1] 0.6621043
# the model is suited for UK

predictTaiwan <- predict(lm1, newdata = taiwanNew)
cor(predictTaiwan, taiwanNew$ConfirmedNew)
# [1] 0.4098046
# the model is not suited for Taiwan

# remove the outlier of china to avoid plot rage too wide
# , which is 15133 confirmNew on 02/13/2020
chinaBigRow <- subset(chinaNew, ConfirmedNew > 10000)
chinaNew <- subset(chinaNew, ConfirmedNew < 10000)

Country <- c("China", "Italy", "Iran", "Spain", "Germany", "US", "France", "South Korea", "Switzerland", "UK", "Taiwan")

# draw the correlations by Linear Model of Iran
CorrelationIran <- c(round(abs(cor(predictChina, chinaNew$ConfirmedNew))*100, digits=2),
                      round(abs(cor(predictItaly, italyNew$ConfirmedNew))*100, digits=2),
                      round(abs(cor(predictIran, testIran$ConfirmedNew))*100, digits=2),
                      round(abs(cor(predictSpain, spainNew$ConfirmedNew))*100, digits=2),
                      round(abs(cor(predictGermany, germanyNew$ConfirmedNew))*100, digits=2),
                      round(abs(cor(predictUs, usNew$ConfirmedNew))*100, digits=2),
                      round(abs(cor(predictFrance, franceNew$ConfirmedNew))*100, digits=2),
                      round(abs(cor(predictKorea, koreaNew$ConfirmedNew))*100, digits=2),
                      round(abs(cor(predictSwitzerland, switzerlandNew$ConfirmedNew))*100, digits=2),
                      round(abs(cor(predictUK, ukNew$ConfirmedNew))*100, digits=2),
                      round(abs(cor(predictTaiwan, taiwanNew$ConfirmedNew))*100, digits=2))


iranCor <- data.frame(Country=Country, Correlation=CorrelationIran)

ggplot(data=iranCor, aes(x=Country, y=Correlation, fill=Country)) + 
  geom_bar(stat="identity") + 
  ggtitle("Correlations by Linear Model of Iran") + 
  ylab("Correlation (%)") +
  geom_text(aes(label=Correlation), vjust=1.6, color="white", size=3.5)


# combine
allNew <- rbind(chinaNew, italyNew, spainNew, germanyNew, iranNew, 
                usNew, franceNew, koreaNew, switzerlandNew, ukNew, taiwanNew)

# transfer country to factor
allNew$Country <- as.factor(allNew$Country)
levels(allNew$Country)
# [1] "China"       "France"      "Germany"     "Iran"        "Italy"       "South Korea" "Spain"      
# [8] "Switzerland" "Taiwan"      "UK"          "US"

# Because ggplot only support 6 shapes once, so we try to compare different region

# by confirmed date
# World Top 6 
top6 <- allNew[allNew$Country %in% c("China", "Italy", "Spain", "Germany", "Iran", "US"),]
ggplot(data=top6,
       mapping=aes(x=ObservationDate, y=ConfirmedNew, shape=Country, color=Country)) + 
  geom_point() + geom_line() + ggtitle("COVID-19 New Confirmed by Date (World Top 6)") + 
  xlab("Confirmed Date") + ylab("Confirmed New")

# Europe
europe <- allNew[allNew$Country %in% c("Spain", "France", "Germany", "Switzerland", "Italy", "UK"),]
ggplot(data=europe, 
       mapping=aes(x=ObservationDate, y=ConfirmedNew, shape=Country, color=Country)) + 
  geom_point() + geom_line() + ggtitle("COVID-19 New Confirmed by Date (Europe)") + 
  xlab("Confirmed Date") + ylab("Confirmed New")

# America and Asia
americaAsia <- allNew[allNew$Country %in% c("China", "US", "Iran", "South Korea", "Taiwan"),]
ggplot(data=americaAsia,
       mapping=aes(x=ObservationDate, y=ConfirmedNew, shape=Country, color=Country)) + 
  geom_point() + geom_line() + ggtitle("COVID-19 New Confirmed by Date (America and Asia)") + 
  xlab("Confirmed Date") + ylab("Confirmed New")


# by confirmed day
# World Top 6 
ggplot(data=top6,
       mapping=aes(x=seq, y=ConfirmedNew, shape=Country, color=Country)) + 
  geom_point() + geom_line() + ggtitle("COVID-19 New Confirmed by Day (World Top 6)") + 
  xlab("Confirmed Day") + ylab("Confirmed New")

# Europe
ggplot(data=europe, 
       mapping=aes(x=seq, y=ConfirmedNew, shape=Country, color=Country)) + 
  geom_point() + geom_line() + ggtitle("COVID-19 New Confirmed by Day (Europe)") + 
  xlab("Confirmed Day") + ylab("Confirmed New")

# America and Asia
ggplot(data=americaAsia,
       mapping=aes(x=seq, y=ConfirmedNew, shape=Country, color=Country)) + 
  geom_point() + geom_line() + ggtitle("COVID-19 New Confirmed by Day (America and Asia)") + 
  xlab("Confirmed Day") + ylab("Confirmed New")


# plot(allNew$ObservationDate, allNew$ConfirmedNew,
#      pch=c(allNew$Country), col=c(allNew$Country),
#      xlab="Observation Date", ylab="Confirmed New",
#      main="COVID-19 Confirmed New by Date")
# legend('topleft', legend=c("China", "Iran", "Italy", "Taiwan", "US"), pch=c(1:5), col=c(1:5))

# to see the trend when coronavirus first occurred in each country
# plot(allNew$no, allNew$ConfirmedNew,
#     pch=c(allNew$Country), col=c(allNew$Country),
#     xlab="Confirmed Day", ylab="Confirmed New",
#     main="COVID-19 Confirmed New by Day")
# legend('topleft', legend=c("China", "Iran", "Italy", "Taiwan", "US"), pch=c(1:5), col=c(1:5))


### since Asia is the COVID original place, use South Korea to run the linear regression
summary(koreaNew)
# ObservationDate        Confirmed        Deaths        Recovered       ConfirmedNew  
# Min.   :2020-01-22   Min.   :   1   Min.   : 0.00   Min.   :   0.0   Min.   :  0.0  
# 1st Qu.:2020-02-05   1st Qu.:  19   1st Qu.: 0.00   1st Qu.:   0.0   1st Qu.:  1.0  
# Median :2020-02-19   Median :  31   Median : 0.00   Median :  12.0   Median :  7.0  
# Mean   :2020-02-19   Mean   :2419   Mean   :18.39   Mean   : 130.6   Mean   :147.6  
# 3rd Qu.:2020-03-04   3rd Qu.:5621   3rd Qu.:35.00   3rd Qu.:  41.0   3rd Qu.:229.0  
# Max.   :2020-03-18   Max.   :8413   Max.   :84.00   Max.   :1540.0   Max.   :851.0  
#   DeathsNew       RecoveredNew      Country               seq    
# Min.   : 0.000   Min.   :-17.00   Length:57          Min.   : 1  
# 1st Qu.: 0.000   1st Qu.:  0.00   Class :character   1st Qu.:15  
# Median : 0.000   Median :  0.00   Mode  :character   Median :29  
# Mean   : 1.474   Mean   : 27.02                      Mean   :29  
# 3rd Qu.: 2.000   3rd Qu.:  2.00                      3rd Qu.:43  
# Max.   :11.000   Max.   :627.00                      Max.   :57

head(koreaNew)
#   ObservationDate Confirmed Deaths Recovered ConfirmedNew DeathsNew RecoveredNew     Country seq
# 1      2020-01-22         1      0         0            0         0            0 South Korea   1
# 2      2020-01-23         1      0         0            0         0            0 South Korea   2
# 3      2020-01-24         2      0         0            1         0            0 South Korea   3
# 4      2020-01-25         2      0         0            0         0            0 South Korea   4
# 5      2020-01-26         3      0         0            1         0            0 South Korea   5
# 6      2020-01-27         4      0         0            1         0            0 South Korea   6

n <- nrow(koreaNew)
ntrain <- round(n * 0.6)
set.seed(206) # set seed for reproductible results
tindex <- sample(n, ntrain)
trainKorea <- koreaNew[tindex,] # training set
testKorea <- koreaNew[-tindex,] # test set

formula <- ConfirmedNew ~ ObservationDate + Confirmed + Deaths + 
  Recovered + DeathsNew + RecoveredNew
lm2 <- lm(formula, data=trainKorea)
summary(lm2)
# Call:
# lm(formula = formula, data = trainKorea)
# 
# Residuals:
#     Min      1Q  Median      3Q     Max 
# -147.84  -46.84  -11.93   42.02  194.53 
# 
# Coefficients:
# Estimate Std. Error t value Pr(>|t|)    
# (Intercept)     -8.793e+04  3.738e+04  -2.352  0.02621 *  
# ObservationDate  4.806e+00  2.043e+00   2.353  0.02616 *  
# Confirmed        1.964e-01  3.283e-02   5.982 2.22e-06 ***
# Deaths          -2.825e+01  4.704e+00  -6.006 2.08e-06 ***
# Recovered        4.000e-01  1.343e-01   2.978  0.00607 ** 
# DeathsNew        2.865e+01  8.467e+00   3.384  0.00220 ** 
# RecoveredNew    -6.087e-01  5.342e-01  -1.139  0.26457    
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 89.6 on 27 degrees of freedom
# Multiple R-squared:  0.782,	Adjusted R-squared:  0.7335 
# F-statistic: 16.14 on 6 and 27 DF,  p-value: 8.396e-08

# R-squared is good, p-values are good, RSE is good

par(mfrow=c(2,2))
plot(lm2)

# the 4 plots are ok

# predict test set
predictKorea <- predict(lm2, newdata = testKorea)
cor(predictKorea, testKorea$ConfirmedNew)
# [1] 0.8181736
# the model is quite ok, strong

predictChina <- predict(lm2, newdata = chinaNew)
cor(predictChina, chinaNew$ConfirmedNew)
# [1] 0.555154
# the model is not suited for China, moderate

predictItaly <- predict(lm2, newdata = italyNew)
cor(predictItaly, italyNew$ConfirmedNew)
# [1] -0.8485371
# the model is suited for Italy, very strong

predictIran <- predict(lm2, newdata = iranNew)
cor(predictIran, iranNew$ConfirmedNew)
# [1] -0.7147556
# the model is suited for Iran, strong

predictSpain <- predict(lm2, newdata = spainNew)
cor(predictSpain, spainNew$ConfirmedNew)
# [1] -0.7331091
# the model is suited for Spain, strong

predictGermany <- predict(lm2, newdata = germanyNew)
cor(predictGermany, germanyNew$ConfirmedNew)
# [1] 0.9622448
# the model is suited for Germany, very strong

predictUs <- predict(lm2, newdata = usNew)
cor(predictUs, usNew$ConfirmedNew)
# [1] -0.9147847
# the model is suited for USy, very strong

predictFrance <- predict(lm2, newdata = franceNew)
cor(predictFrance, franceNew$ConfirmedNew)
# [1] -0.6731894
# the model is suited for France, moderate

predictSwitzerland <- predict(lm2, newdata = switzerlandNew)
cor(predictSwitzerland, switzerlandNew$ConfirmedNew)
# [1] 0.5597923
# the model is not suited for Switzerland, moderate

predictUK <- predict(lm2, newdata = ukNew)
cor(predictUK, ukNew$ConfirmedNew)
# [1] -0.6594296
# the model is suited for UK, moderate

predictTaiwan <- predict(lm2, newdata = taiwanNew)
cor(predictTaiwan, taiwanNew$ConfirmedNew)
# [1] 0.4427289
# the model is not suited for Taiwan, poor

Country <- c("China", "Italy", "Iran", "Spain", "Germany", "US", "France", "South Korea", "Switzerland", "UK", "Taiwan")

# draw the correlations by Linear Model of South Korea
CorrelationKorea <- c(round(cor(predictChina, chinaNew$ConfirmedNew)*100, digits = 2),
                      round(abs(cor(predictItaly, italyNew$ConfirmedNew))*100, digits = 2),
                      round(abs(cor(predictIran, iranNew$ConfirmedNew))*100, digits = 2),
                      round(abs(cor(predictSpain, spainNew$ConfirmedNew))*100, digits = 2),
                      round(cor(predictGermany, germanyNew$ConfirmedNew)*100, digits = 2),
                      round(abs(cor(predictUs, usNew$ConfirmedNew))*100, digits = 2),
                      round(abs(cor(predictFrance, franceNew$ConfirmedNew))*100, digits = 2),
                      round(cor(predictKorea, testKorea$ConfirmedNew)*100, digits = 2),
                      round(cor(predictSwitzerland, switzerlandNew$ConfirmedNew)*100, digits = 2),
                      round(abs(cor(predictUK, ukNew$ConfirmedNew))*100, digits = 2),
                      round(cor(predictTaiwan, taiwanNew$ConfirmedNew)*100, digits = 2))


koreaCor <- data.frame(Country=Country, Correlation=CorrelationKorea)

ggplot(data=koreaCor, aes(x=Country, y=Correlation, fill=Country)) + 
  geom_bar(stat="identity") + 
  ggtitle("Correlations by Linear Model of South Korea") + 
  ylab("Correlation (%)") +
  geom_text(aes(label=Correlation), vjust=1.6, color="white", size=3.5)


# 8. Prepare a report using techniques of “data storytelling” to present the results to a 
# management-level audience – state the goals of the project, the data sets used, EDA results, 
# data visualization, overview of how you used machine learning algorithms, and final conclusions.

# use PDF to describe this
