library(geojson)
library(geojsonio)
library(geojsonsf)
library(sp)
library(ggplot2)
library(leaflet)
library(sf)
library(dplyr)
library(tibble)
library(tidyverse)

#East of England COVID data from GOV
East_England_COVID <- read.csv("https://api.coronavirus.data.gov.uk/v2/data?areaType=msoa&areaCode=E12000006&metric=newCasesBySpecimenDateRollingSum&metric=newCasesBySpecimenDateRollingRate&format=csv")

#Replace NA's in the COVID data to 0's
index_NAsTo0 <- is.na(East_England_COVID)
East_England_COVID[index_NAsTo0] <- 0

#Find COVID data for Upper Tier Local Authority Hertfordshire
Herts_COVID <- subset(East_England_COVID, UtlaName == "Hertfordshire")

#Shorten the COVID data column names to something more presentable
names(Herts_COVID)[names(Herts_COVID) == 
                            'newCasesBySpecimenDateRollingSum'] <- 'NewCases'
names(Herts_COVID)[names(Herts_COVID) == 
                            'newCasesBySpecimenDateRollingRate'] <- 'NewCaseRate'

#Create dataframes for each Lower Tier Local Authorities, Broxbourne to Welwyn Hatfield,
#In these dataframes, aggregate the COVID cases by date
#In the case of COVID cases, sum... with COVID rates, mean
#By aggregating by date, we account for each neighborhood within the Ltla for each specific week
#i.e. Stevenage includes St Nichs, Martins Wood, Woodford Old Town etc.
#We can track the cumulative cases and the case rates for each ltla in Herts

Broxbourne <- subset(Herts_COVID, LtlaName == "Broxbourne")
Broxbourne_TC <- aggregate(NewCases ~ date, data = Broxbourne , FUN = sum)
Broxbourne_MCR <- aggregate(NewCaseRate ~ date, data = Broxbourne, FUN = mean)
names(Broxbourne_TC)[names(Broxbourne_TC) == 
                       'NewCases'] <- 'Broxbourne'
names(Broxbourne_MCR)[names(Broxbourne_MCR) == 
                        'NewCaseRate'] <- 'Broxbourne'

Dacorum <- subset(Herts_COVID, LtlaName == "Dacorum")
Dacorum_TC <- aggregate(NewCases ~ date, data = Dacorum, FUN = sum)
Dacorum_MCR <- aggregate(NewCaseRate ~ date, data = Dacorum, FUN = mean)
names(Dacorum_TC)[names(Dacorum_TC) == 
                    'NewCases'] <- 'Dacorum'
names(Dacorum_MCR)[names(Dacorum_MCR) == 
                    'NewCaseRate'] <- 'Dacorum'

East_Hertfordshire <- subset(Herts_COVID, LtlaName == "East Hertfordshire")
East_Hertfordshire_TC <- aggregate(NewCases ~ date, data = East_Hertfordshire, FUN = sum)
East_Hertfordshire_MCR <- aggregate(NewCaseRate ~ date, data = East_Hertfordshire, FUN = mean)
names(East_Hertfordshire_TC)[names(East_Hertfordshire_TC) == 
                               'NewCases'] <- 'East Hertfordshire'
names(East_Hertfordshire_MCR)[names(East_Hertfordshire_MCR) == 
                               'NewCaseRate'] <- 'East Hertfordshire'

Hertsmere <- subset(Herts_COVID, LtlaName == "Hertsmere")
Hertsmere_TC <- aggregate(NewCases ~ date, data = Hertsmere, FUN = sum)
Hertsmere_MCR <- aggregate(NewCaseRate ~ date, data = Hertsmere, FUN = mean)
names(Hertsmere_TC)[names(Hertsmere_TC) == 
                      'NewCases'] <- 'Hertsmere'
names(Hertsmere_MCR)[names(Hertsmere_MCR) == 
                      'NewCaseRate'] <- 'Hertsmere'

North_Hertfordshire <- subset(Herts_COVID, LtlaName == "North Hertfordshire")
North_Hertfordshire_TC <- aggregate(NewCases ~ date, data = North_Hertfordshire, FUN = sum)
North_Hertfordshire_MCR <- aggregate(NewCaseRate ~ date, data = North_Hertfordshire, FUN = mean)
names(North_Hertfordshire_TC)[names(North_Hertfordshire_TC) == 
                                'NewCases'] <- 'North Hertfordshire'
names(North_Hertfordshire_MCR)[names(North_Hertfordshire_MCR) == 
                                'NewCaseRate'] <- 'North Hertfordshire'


St_Albans <- subset(Herts_COVID, LtlaName == "St Albans")
St_Albans_TC <- aggregate(NewCases ~ date, data = St_Albans, FUN = sum)
St_Albans_MCR <- aggregate(NewCaseRate ~ date, data = St_Albans, FUN = mean)
names(St_Albans_TC)[names(St_Albans_TC) == 
                      'NewCases'] <- 'St Albans'
names(St_Albans_MCR)[names(St_Albans_MCR) == 
                      'NewCaseRate'] <- 'St Albans'


Stevenage <- subset(Herts_COVID, LtlaName == "Stevenage")
Stevenage_TC <- aggregate(NewCases ~ date, data = Stevenage, FUN = sum)
Stevenage_MCR <- aggregate(NewCaseRate ~ date, data = Stevenage, FUN = mean)
names(Stevenage_TC)[names(Stevenage_TC) == 
                      'NewCases'] <- 'Stevenage'
names(Stevenage_MCR)[names(Stevenage_MCR) == 
                      'NewCaseRate'] <- 'Stevenage'


Three_Rivers <- subset(Herts_COVID, LtlaName == "Three Rivers")
Three_Rivers_TC <- aggregate(NewCases ~ date, data = Three_Rivers, FUN = sum)
Three_Rivers_MCR <- aggregate(NewCaseRate ~ date, data = Three_Rivers, FUN = mean)
names(Three_Rivers_TC)[names(Three_Rivers_TC) == 
                         'NewCases'] <- 'Three Rivers'
names(Three_Rivers_MCR)[names(Three_Rivers_MCR) == 
                         'NewCaseRate'] <- 'Three Rivers'

Watford <- subset(Herts_COVID, LtlaName == "Watford")
Watford_TC <- aggregate(NewCases ~ date, data = Watford, FUN = sum)
Watford_MCR <- aggregate(NewCaseRate ~ date, data = Watford, FUN = mean)
names(Watford_TC)[names(Watford_TC) == 
                    'NewCases'] <- 'Watford'
names(Watford_MCR)[names(Watford_MCR) == 
                    'NewCaseRate'] <- 'Watford'

Welwyn_Hatfield <- subset(Herts_COVID, LtlaName == "Welwyn Hatfield")
Welwyn_Hatfield_TC <- aggregate(NewCases ~ date, data = Welwyn_Hatfield, FUN = sum)
Welwyn_Hatfield_MCR <- aggregate(NewCaseRate ~ date, data = Welwyn_Hatfield, FUN = mean)
names(Welwyn_Hatfield_TC)[names(Welwyn_Hatfield_TC) == 
                            'NewCases'] <-  'Welwyn Hatfield'
names(Welwyn_Hatfield_MCR)[names(Welwyn_Hatfield_MCR) == 
                            'NewCaseRate'] <- 'Welwyn Hatfield'
      
#Inefficient... merge only works with two dataframes, not sure on Reduce() function                     
Herts_Weekly_Cases <- cbind(Broxbourne_TC, Dacorum_TC, East_Hertfordshire_TC, 
                            Hertsmere_TC, North_Hertfordshire_TC, St_Albans_TC, 
                            Stevenage_TC, Three_Rivers_TC, Watford_TC, 
                            Welwyn_Hatfield_TC)
Herts_Weekly_Cases <- Herts_Weekly_Cases[-c(3,5,7,9,11,13,15,17,19)]
#Using Cbind means there's 11 date columns... merge by date would change this...
#Will do for now

#Same as above with weekly mean rates, same issue
Herts_Weekly_Rates <- cbind(Broxbourne_MCR, Dacorum_MCR, East_Hertfordshire_MCR, 
                            Hertsmere_MCR, North_Hertfordshire_MCR, St_Albans_MCR,
                            Stevenage_MCR, Three_Rivers_MCR, Watford_MCR,
                            Welwyn_Hatfield_MCR)
Herts_Weekly_Rates <- Herts_Weekly_Rates[-c(3,5,7,9,11,13,15,17,19)]

#To begin plotting graphs, the date needs to be classified as DATE
Herts_Weekly_Cases$date <- as.Date(Herts_Weekly_Cases$date, format = "%Y-%m-%d")
Herts_Weekly_Rates$date <- as.Date(Herts_Weekly_Rates$date, format = "%Y-%m-%d")

#First attempt - produces a basic line graph, but at this point only one line can be plotted
Stevenage_Test <- ggplot(data = Herts_Weekly_Cases, aes(x = date, y = `Stevenage_TC`)) + 
  geom_line() + geom_point()

#In order to plot all lines in the dataframe, the dataframe needs to be reshaped
#from wide to long using the function melt()
#https://stackoverflow.com/questions/3777174/plotting-two-variables-as-lines-using-ggplot2-on-the-same-graph

library(reshape)

Herts_Weekly_Cases_Long <- melt(Herts_Weekly_Cases, id = "date")
names(Herts_Weekly_Cases_Long)[names(Herts_Weekly_Cases_Long) ==
                                 "variable"] <- "Local_Authority" #Rename variable column Local Authority
names(Herts_Weekly_Cases_Long)[names(Herts_Weekly_Cases_Long) ==
                                 "value"] <- "Cumulative_Cases"  #Rename Value column Cumulative Cases
                                 
Herts_Weekly_Rates_Long <- melt(Herts_Weekly_Rates, id = "date")
names(Herts_Weekly_Rates_Long)[names(Herts_Weekly_Rates_Long) ==
                                 "variable"] <- "Local_Authority"
names(Herts_Weekly_Rates_Long)[names(Herts_Weekly_Rates_Long) ==
                                 "value"] <- "Case_Rate"

Herts_Case_Graph <-ggplot(data = Herts_Weekly_Cases_Long, 
                          aes(x = date, y = Cumulative_Cases, color = Local_Authority)) + 
                          geom_line() + xlab("Date\n") + ylab("Weekly Cases\n") + 
                          labs(color = "Local Authority\n") + 
                          ggtitle("Cumulative Weekly Cases in each Hertfordshire Local Authority, 07/20 - Present\n")
#to note, the \n spaces the axis/legend TITLES from the actual axis/legend                         

Herts_Rate_Graph <-ggplot(data = Herts_Weekly_Rates_Long, 
                          aes(x = date, y = Case_Rate, color = Local_Authority)) + 
                          geom_line() + xlab("Date\n") + 
                          ylab("Weekly Case Rate (per 100,000)\n") +
                          labs(color = "Local Authority\n") + 
                          ggtitle("Weekly Case Rate in each Hertfordshire Local Authority, 07/20 - Present\n")

#How about we try to make these somewhat interactive 
#https://cengel.github.io/R-data-viz/interactive-graphs.html

library(plotly)

HCG_Interactive <- ggplotly(Herts_Case_Graph)
HRG_Interactive <- ggplotly(Herts_Rate_Graph)
#Hover labels portray the necessary data


#Spaghetti plot multiple charts
Herts_Cases_Spaghetti <- ggplot(data = Herts_Weekly_Cases_Long, 
                         aes(x = date, y = Cumulative_Cases, color = Local_Authority)) + 
                         geom_line() + 
                         facet_wrap(~Local_Authority) + 
                         labs(color = "Local Authority\n") +
                         xlab("Date\n") + ylab("Weekly Case Rate\n") +
                         ggtitle("Cumulative Weekly Cases in each Hertfordshire Authority, 07/20 - Present\n")


Herts_Rates_Spaghetti <- ggplot(data = Herts_Weekly_Rates_Long, 
                         aes(x = date, y = Case_Rate, color = Local_Authority)) + 
                         geom_line() + 
                         facet_wrap(~Local_Authority) + 
                         labs(color = "Local Authority\n") +
                         xlab("Date\n") + ylab("Weekly Case Rate\n") +
                         ggtitle("Weekly Case Rate in each Hertfordshire Authority, 07/20 - Present\n")
                        
#The small multiple charts is a tad messy, but has potential
ggplotly(Herts_Rates_Spaghetti)
