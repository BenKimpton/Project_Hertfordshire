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

# 06/07/2020 - 

#UK MSOA boundaries
UKsf <- geojsonsf::geojson_sf("https://opendata.arcgis.com/datasets/826dc85fb600440889480f4d9dbb1a24_3.geojson")

#MSOA IMD scores
UK_IMD <- read.csv("https://research.mysociety.org/sites/imd2019/media/data/imd2019_msoa_level_data.csv")

#East of England COVID cases and rates from 06/07/20
East_England_COVID <- read.csv("https://api.coronavirus.data.gov.uk/v2/data?areaType=msoa&areaCode=E12000006&metric=newCasesBySpecimenDateRollingSum&metric=newCasesBySpecimenDateRollingRate&format=csv")

#Change NA's in COVID data to 0's   
index_NAsTo0 <- is.na(East_England_COVID)
East_England_COVID[index_NAsTo0] <- 0

#Shorten COVID stat column names
names(East_England_COVID)[names(East_England_COVID) == 
                          'newCasesBySpecimenDateRollingSum'] <- 'NewCases'
names(East_England_COVID)[names(East_England_COVID) == 
                          'newCasesBySpecimenDateRollingRate'] <- 'NewCaseRate'

#Use chain/pipe operator to create dataframe where Cases have been summed and the 
#average case rate has been calculated for each MSOA covering the entire period
East_England_COVID_Period <- select(East_England_COVID, UtlaName, UtlaCode, 
                                    LtlaName, areaCode, areaName, date, 
                                    NewCases, NewCaseRate) %>%
                             group_by(UtlaName, UtlaCode ,LtlaName,areaCode, 
                                      areaName) %>% 
                             summarise(NewCases = sum(NewCases), 
                                       NewCaseRate = mean(NewCaseRate)) 

#Rename COVID columns in new data frame
names(East_England_COVID_Period)[names(East_England_COVID_Period) == 
                                'NewCases'] <- 'TotalCases'
names(East_England_COVID_Period)[names(East_England_COVID_Period) == 
                                'NewCaseRate'] <- 'MeanCaseRate'

#Create dataframe consisting of IMD and population for East of England
East_England_POP_IMD <- subset(UK_IMD, REG == "East of England", 
                      select = c(LAD19N, REG, POPMID15, IMD19.SCORE))

#Further subset data frame to obtain herts IMD and pop
Herts_POP_IMD <- subset(East_England_POP_IMD, LAD19N %in% 
                        c("Broxbourne", "Dacorum", "East Hertfordshire", 
                          "Hertsmere", "North Hertfordshire", "St Albans",
                          "Stevenage", "Three Rivers", "Watford",
                          "Welwyn Hatfield"),
                           select = c(LAD19N, POPMID15, IMD19.SCORE))

#Subset COVID dataframe to obtain data relating to Hertfordshire 
Herts_COVID <- subset(East_England_COVID_Period, UtlaName == "Hertfordshire")

#Find hertfordshire MSOA boundaries (is there a bettter way to do this?... the way
#i've done it assumes no other MSOA boundaries are removed or added)
Herts_sf <- data.frame(UKsf[4735:4887, 2,7])

#Combine the three Hertfordshire dataframes
Herts_All_Variables <- cbind(Herts_COVID, Herts_POP_IMD, Herts_sf) %>% 
                       select(-LAD19N)

#Next process is to normalize the total cases, mean rate and IMD columns 
#https://www.datasciencemadesimple.com/scaling-or-normalizing-the-column-in-r-2/
#https://stackoverflow.com/questions/24222132/why-i-still-get-number-bigger-than-1-when-i-normalize-data-in-r

# Normalize TotalCases, mean case rate and IMD
Herts_All_Variables$TC_norm <- scale(Herts_All_Variables$TotalCases) 
Herts_All_Variables$MCR_Norm <- scale(Herts_All_Variables$MeanCaseRate)
Herts_All_Variables$IMD_Norm <- scale(Herts_All_Variables$IMD19.SCORE)

Chloropleth_TotalCases <- ggplot(Herts_All_Variables$geometry) + 
                          geom_sf(aes(fill = Herts_All_Variables$TotalCases)) + 
                          scale_fill_gradient(breaks = c(100,200,300,400,500,600),low = "white", high = "darkblue") +
                          guides(fill=guide_colorbar(title="Total Cases")) +
                          ggtitle(("Hertfordshire Total Cases 09/07/20 - 31/12/20"))

Chloropleth_MeanCaseRate <- ggplot(Herts_All_Variables$geometry) + 
                            geom_sf(aes(fill = Herts_All_Variables$MeanCaseRate)) + 
                            scale_fill_gradient(breaks = c(50,100,150,200,250),low = "white", high = "darkgreen") +
                            guides(fill=guide_colorbar(title="Mean Case Rate")) +
                            ggtitle(("Hertfordshire Mean Case Rate 09/07/20 - 31/12/20"))           
  
Chloropleth_IMD <- ggplot(Herts_All_Variables$geometry) + 
                   geom_sf(aes(fill = Herts_All_Variables$IMD19.SCORE)) + 
                   scale_fill_gradient(breaks=c(5,10,15,20,25,30,35),low = "white", high = "darkred") +
                   guides(fill=guide_colorbar(title="IMD Score")) +
                   ggtitle(("Hertfordshire IMD Scores 2019"))

#Try to make these maps interactive

library(plotly)

Test_IMD <- ggplotly(Chloropleth_IMD)
#Works, but the hover labels need adjusting... coding for original chloropleths maps 
#need tidying to do this
  
#linear regression models... need to familiarise myself with the necessary statistical tests and how to conduct them

L_model_TC <- lm(Herts_All_Variables$TotalCases ~ Herts_All_Variables$IMD19.SCORE)

L_model_MCR <- lm(Herts_All_Variables$MeanCaseRate ~ Herts_All_Variables$IMD19.SCORE)