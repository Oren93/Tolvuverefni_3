library(tidyverse)
library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)
oo <- read.csv(file = 'data98.csv', sep =',')
oo <- subset(oo, select = -c(recid,vf))
# Change the names to more usefull names
oo = oo%>%rename("area" = "reit",
                   "sub_area" = "smrt",
                   "trawl_num" = "tog_nr",
                   "day" = "dag",
                   "month" = "man",
                   "min_depth" = "dyp_min",
                   "max_depth" = "dyp_max",
                   "fish_num" = "nr",
                   "fish_length" = "le",
                   "fish_gender" = "ky",
                   "breeding_age" = "kt",
                   "fish_age" = "aldur",
                   "fish_mass" = "osl",
                   "gutted_mass" = "sl",
                   "liver_mass" = "li")
# Function to determine the longtitude and latitude
r2d <-function(r)
{
    lat <- floor(r/100)
    lon <- (r - lat * 100) %% 50
    halfb <- (r - 100 * lat - lon)/100
    lon <-  - (lon + 0.5)
    lat <- lat + 60 + halfb + 0.25
    data.frame(lat = lat, lon = lon)
}

reitir<-oo$area
long <- r2d(reitir)$lon
lat <- r2d(reitir)$lat
oo <- cbind(oo, long, lat)
plot(long,lat,type='n')
text(long,lat,as.character(reitir))


oo <- oo %>%
    mutate(
        quadrant = case_when(
            long >=(-19) & lat >= 65  ~ "NE",
            long <(-19) & lat >= 65  ~ "NW",
            long <(-19) & lat < 65  ~ "SW",
            TRUE ~ "SE"))

rm(lat,long,reitir)

oo <- mutate(oo, breeding_age2 = case_when(
    breeding_age > 1 ~ TRUE,
    breeding_age == 1  ~ FALSE
))

# b

nrow(oo[oo$breeding_age2, oo$quadrant == "SW",])
nrow(oo[oo$quadrant = "SE",])
nrow(subset(oo, breeding_age2==TRUE,fish_mass>2000))
#)

tibble(nrow(filter(oo, breeding_age2 ==FALSE, quadrant == "NE")),
       nrow(filter(oo, breeding_age2 ==FALSE, quadrant == "NW")),
       nrow(filter(oo, breeding_age2 ==FALSE, quadrant == "SW")),
       nrow(filter(oo, breeding_age2 ==FALSE, quadrant == "SE")))
       