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
                   "sex_maturity" = "kt",
                   "fish_age" = "aldur",
                   "fish_mass" = "osl",
                   "gutted_mass" = "sl",
                   "liver_mass" = "li")

r2d <-function(r)
{
    lat <- floor(r/100)
    lon <- (r - lat * 100) %% 50
    halfb <- (r - 100 * lat - lon)/100
    lon <-  - (lon + 0.5)
    lat <- lat + 60 + halfb + 0.25
    data.frame(lat = lat, lon = lon)
}
reitir<-unique(oo$area)
reitir<-oo$area
# x<-r2d(reitir)$lon
# y<-r2d(reitir)$lat
long <- r2d(reitir)$lon
lat <- r2d(reitir)$lat
oo <- cbind(oo, long, lat)
plot(long,lat,type='n')
text(long,lat,as.character(reitir))


oo <- oo %>% mutate(season = recode_factor(long, lat, "1"="NE", "2"="SE",
                                           "12"="SW", "3"="NW"))
oo <- mutate(oo, relative_area = case_when(
    long >=65 && lat >= -19  ~ "NE",
    long <65 && lat >= -19  ~ "NW",
    long <65 && lat < -19  ~ "SW",
    long >=65 && lat < -19  ~ "SE"
    ))
rm(lat,long,reitir,x,y)


