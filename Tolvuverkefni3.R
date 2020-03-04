library(tidyverse)
library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)
oo <- read.csv(file = 'data98.csv', sep =',')
oo <- subset(oo, select = -c(vf))
oo = oo %>% rename("a" = "recid",
oo <- oo %>% mutate(season = recode_factor(month, "1"="winter", "2"="winter",
                                           "12"="winter", "3"="spring",
                                           "4"="spring","5"="spring","6"="summer","7"="summer",
                                           "8"="summer","9"="autumn","10"="autumn","11"="autumn"))


                   "area" = "reit",
                   "sub_area" = "smrt",
                   "trawl_num" = "tog_nr",
                   "day" = "dag",
                   "month" = "man",
                   "min_depth" = "dyp_min",
                   "max_depth" = "dyp_max",
                   "equipment" = "vf",
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
reitir<-unique(oo$reit)
x<-r2d(reitir)$lon
y<-r2d(reitir)$lat
plot(x,y,type='n')
text(x,y,as.character(reitir))
