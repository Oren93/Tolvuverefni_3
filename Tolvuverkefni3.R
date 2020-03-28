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

# b (not finished)

#nrow(oo[oo$breeding_age2, oo$quadrant == "SW",])
#nrow(oo[oo$quadrant = "SE",])
#nrow(subset(oo, breeding_age2==TRUE,fish_mass>2000))

#
NE <- c(nrow(filter(oo, breeding_age2 ==FALSE, quadrant == "NE")),
        nrow(filter(oo, breeding_age2 ==TRUE, quadrant == "NE")))
NW <- c(nrow(filter(oo, breeding_age2 ==FALSE, quadrant == "NW")),
        nrow(filter(oo, breeding_age2 ==TRUE, quadrant == "NW")))
SW <- c(nrow(filter(oo, breeding_age2 ==FALSE, quadrant == "SW")),
        nrow(filter(oo, breeding_age2 ==TRUE, quadrant == "SW")))
SE <- c(nrow(filter(oo, breeding_age2 ==FALSE, quadrant == "SE")),
        nrow(filter(oo, breeding_age2 ==TRUE, quadrant == "SE")))
adulthood <- c('Breeding', 'Non_breeding')
count <- tibble(adulthood,NE,NW,SW,SE)

knitr::kable(tibble(adulthood,NE,NW,SW,SE),
      align = 'ccc', table.attr = "class=\"table\"", 
      format = "html")

count <- gather(count,key=area, value=count,
             c(NE,NW,SW,SE))

ggplot(count, aes(fill=adulthood, y=count, x=area)) + 
  geom_bar(position="dodge", stat="identity") +
  scale_fill_manual(values=c("#26dbff", "#fff700"))+ theme_linedraw()+
  labs(title="Number of fish by sea quadrant",x="quadrant")
rm(adulthood, NE,NW,SE,SW,count)

# C)
C_Vector <- c(nrow(oo),mean(oo$fish_length), mean(oo$fish_mass))
age_ordered <- tibble(length = oo$fish_length,age = oo$fish_age)[order(oo$fish_age),]
sd(age_ordered$length)

sd_by_age <- c()
age <- c()
for (i in 1: max(age_ordered$age)) {
  x <- filter(age_ordered, age == i)
  sd_by_age <- append(sd_by_age, sd(x$length))
  age <- append(age, nrow(x))
}
rm(x)


ggplot(age_ordered, aes(x = age, y = length))+
  geom_point()+
  geom_smooth(method = "loess") 


# d)
#Creates a dataframe with 100 random fish from two random quadrants
#temp column
oo <- oo %>% mutate(quadrant_num = recode_factor
    (quadrant, "NE"="1", "NW"="2",
    "SW"="3", "SE"="4"))

#creates temp dataframe with random quadrants
set.seed(0601)
q1 = filter(oo, quadrant_num == toString(floor(runif(1, min=0, max=4))))
set.seed(0601)
q2 = filter(oo, quadrant_num == toString(ceiling(runif(1, min=1, max=5))))

#takes 50 random values from the temp dataframes
set.seed(0601)
q1 = sample_n(q1 ,50)
set.seed(0601)
q2 = sample_n(q2 ,50)

#combines temp dataframes
fish_tbl = rbind(q1, q2)



