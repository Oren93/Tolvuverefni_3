library(tidyverse)
library(readr)
library(dplyr)
library(lubridate) # pakki med follum fyrir vinnslu med dagsetningar (date eda datetime breytur)
library(ggplot2)
oo <- read.csv(file = 'data98.csv', sep =',')
oo = oo %>% rename("a" = "recid",
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
