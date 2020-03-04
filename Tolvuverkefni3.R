library(tidyverse)
library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)
oo <- read.csv(file = 'data98.csv', sep =',')
oo <- subset(oo, select = -c(vf))
oo = oo %>% rename("a" = "recid",
                   "b" = "reit",
                   "c" = "smrt",
                   "d" = "tog_nr",
                   "e" = "dag",
                   "f" = "man",
                   "g" = "dyp_min",
                   "h" = "dyp_max",
                   "j" = "nr",
                   "k" = "le",
                   "l" = "ky",
                   "m" = "kt",
                   "n" = "aldur",
                   "o" = "osl",
                   "p" = "sl",
                   "q" = "li")
oo <- oo %>% mutate(season = recode_factor(month, "1"="winter", "2"="winter",
                                           "12"="winter", "3"="spring",
                                           "4"="spring","5"="spring","6"="summer","7"="summer",
                                           "8"="summer","9"="autumn","10"="autumn","11"="autumn"))


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
