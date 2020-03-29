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

#count$adulthood <- as.numeric(as.character(yyz$b))
count <- ddply(count,.(area),transform,percent = 100*count/sum(count))
#count <- count %>% mutate(percent = count/total)


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
rm(x,i)


age_ordered <- ddply(age_ordered,.(age),transform,length.New = mean(length))
ggplot(age_ordered, aes(x = age, y = length))+
  geom_point(data = age_ordered, aes( y = length.New), size = 4, 
           shape = 21, fill = "red")+
  geom_smooth(method = "loess") 

ggplot(plot.data, aes(x=age,y=length)) + geom_bar(position="dodge", stat="identity")+
  theme_linedraw() + labs(title="Length of fish by age")


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

#combines temp dataframes, removes temp dataframes
fish_tbl = rbind(q1, q2)
rm(q1, q2)


# F
library(reshape2)
oo_long = melt(oo, id.vars='quadrant', measure.vars='fish_length', value.name='fish_length')
# Skilgreinum fall sem ad tekur inn gagnavigur x og
#   skilar gognum sem fylgja "natturulegu" normaldreifingu vigursins x
get_normal_density <- function(x, binwidth) {
  grid <- seq(min(x), max(x), length=100)
  data.frame(
    fish_length = grid,
    normal_curve = dnorm(grid, mean(x), sd(x)) * length(x) * binwidth
  )
}

# Skilgreinum breytu fyrir binwidth
BW <- 3

# Buum til normaldreifd gogn fyrir hvert hafsvaedi med thvi ad
#   beita fallinu "get_normal_density" a lengdarmaelingar sem tilheyra
#   hverju hafsvaedi fyrir sig
normaldens <-
  oo %>%
  group_by(quadrant) %>%
  do(get_normal_density(x=.$fish_length, binwidth=BW))

ggplot() + geom_histogram(data=oo_long, aes(x=fish_length),binwidth=BW)+
  geom_line(data=normaldens,mapping=aes(x=fish_length,y=normal_curve),col="red",size=1)+
  facet_wrap(~quadrant)+
  labs(x="length")

rm(age,BW,get_normal_density,C_Vector,sd_by_age,oo_long,normaldens,age_ordered)
#-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-
# the following 2 plot are not to be used

ggplot(oo, aes( x=fish_length)) + 
  geom_histogram()+xlab("Price (thousands - ISK)")+ylab("Frequency - Number of properties")+
  labs(title="Length of fish by age",x="length",y="count")+
  geom_line(stat="count", col = "red")+
  #geom_line(data = oo, aes(x = fish_length, y = n), col = "red")
  theme_linedraw()

  ggplot(oo, aes(fish_length)) +
    geom_histogram(stat="count", position = "dodge") + 
    geom_line(stat="count", col = "red")+
    scale_fill_brewer(palette = "Set1")+
    theme_linedraw()
#-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-

  
#################################################################
# Bonus attempt
  # need to sort out and remove useless libraries
#library(tidyverse)
#library(knitr)    
#library(broom)
#library(stringr)
#library(modelr)
#library(forcats)
#library(ggmap)
#library(maps)
library(sf)## for the different approach
theme_set(theme_bw())
library("sf")
#library("sp")
library("rnaturalearth")##
library("rnaturalearthdata")##
#library("rgeos")

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

## not related. beautiful population size map of the world
ggplot(data = world) +
  geom_sf(aes(fill = pop_est)) +
  scale_fill_viridis_c(option = "plasma", trans = "sqrt")
## blue map of iceland
ggplot(data = world) +
  geom_sf(color = "black", fill = "blue")+
  coord_sf(xlim = c(min(oo$long), max(oo$long)), ylim = c(min(oo$lat),max(oo$lat)), expand = TRUE)+
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("World map", subtitle = paste0("(", length(unique(world$NAME)), " countries)"))

##
reitir<-unique(oo$area)
x<-r2d(reitir)$lon
y<-r2d(reitir)$lat
sites <- data.frame(longitude=x, latitude = y)

## Icelandic map with fish coordinates (need to beautify, text overlaps)
ggplot(data = world) +
  geom_sf(color = "black", fill = "blue")+
  geom_point(data = sites, aes(x = longitude, y = latitude), size = 4, 
             shape = 23, fill = "yellow") +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("World map", subtitle = paste0("(", length(unique(world$NAME)), " countries)"))+
  coord_sf(xlim = c(min(oo$long), max(oo$long)), ylim = c(min(oo$lat),max(oo$lat)), expand = TRUE)+
geom_text(data=sites,aes(x=longitude,y=latitude,label=
                          paste0("(", x,",",y)),hjust=0, vjust=0)
#geom_text(data=sites,aes(x=longitude,y=latitude,label=unique(oo$area)),hjust=0, vjust=0)

#different approach
sites <- st_as_sf(sites, coords = c("longitude", "latitude"), 
                  crs = 4326, agr = "constant")

ggplot(data = world) +
  geom_sf(color = "black", fill = "blue") +
  geom_sf(data = sites, size = 4, shape = 23, fill = "red") +
  ggtitle("Fish around Iceland", subtitle = paste0("(", length(unique(world$NAME)), " countries)"))+
  coord_sf(xlim = c(min(oo$long), max(oo$long)), ylim = c(min(oo$lat),max(oo$lat)), expand = TRUE)
