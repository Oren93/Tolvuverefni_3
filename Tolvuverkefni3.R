library(tidyverse)
library(readr)
library(dplyr)
library(lubridate)
library(ggplot2)
# really cool staff:
theme_set(theme_dark()+theme(
  plot.background = element_rect(fill = "#929292"),
  plot.title = element_text(color="white", size=18, face="bold"),
  panel.background = element_rect(fill = "#bdcdff",
                                  colour = "lightblue",
                                  size = 0.5, linetype = "solid"),
  panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                  colour = "white"), 
  panel.grid.minor = element_line(size = 0.25, linetype = 'dashed',
                                  colour = "white"),
  axis.text = element_text(color="#ffffff")
))


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
NE <- c(nrow(filter(oo, breeding_age2 ==FALSE, quadrant == "NE")),
        nrow(filter(oo, breeding_age2 ==TRUE, quadrant == "NE")))
NW <- c(nrow(filter(oo, breeding_age2 ==FALSE, quadrant == "NW")),
        nrow(filter(oo, breeding_age2 ==TRUE, quadrant == "NW")))
SW <- c(nrow(filter(oo, breeding_age2 ==FALSE, quadrant == "SW")),
        nrow(filter(oo, breeding_age2 ==TRUE, quadrant == "SW")))
SE <- c(nrow(filter(oo, breeding_age2 ==FALSE, quadrant == "SE")),
        nrow(filter(oo, breeding_age2 ==TRUE, quadrant == "SE")))
adulthood <- c('Breeding', 'Non_breeding')
# table of quadrant, count, and percent of adulthood per region
fish_count <- tibble(adulthood,NE,NW,SW,SE)
fish_count <- gather(fish_count,key=area, value=count,
                c(NE,NW,SW,SE))
library(plyr) # needed only for ddply, to get the percentage of fish when a quadrant is the whole
fish_count <- ddply(fish_count,.(area),transform,percent = 100*count/sum(count))
unloadNamespace("plyr") # # unloading because everything else breaks while it's loaded.



knitr::kable(tibble(adulthood,NE,NW,SW,SE),
      align = 'ccc', table.attr = "class=\"table\"", 
      format = "html")


#percent of fish of breeding age by quadrant
ggplot(fish_count, aes(fill=adulthood, y=percent, x=area)) + 
  geom_bar(position="dodge", stat="identity") +
  scale_fill_manual(values=c("#26dbff", "#fff700"))+
  labs(title="Percent of adult and young fish per quadrant",subtitle = "(total in each quadrant is 100%)",
       x="quadrant")

#amount of fish of breeding age by quadrant
ggplot(fish_count, aes(fill=adulthood, y=count, x=area)) + 
  geom_bar(position="dodge", stat="identity") +
  scale_fill_manual(values=c("#26dbff", "#fff700"))+
  labs(title="Number of fish by sea quadrant",x="quadrant")

rm(adulthood, NE,NW,SE,SW,fish_count)

# C)
# better to have it ordered for code later to run faster
age_ordered <- tibble(length = oo$fish_length,weight = oo$fish_mass,
                      age = oo$fish_age)[order(oo$fish_age),]
# I think we need to print that value:
sd(age_ordered$length)

# creating empty vectors to be fed in a loop
count_by_age <- c()
AvgW_by_age <- c()
AvgL_by_age <- c()
sd_by_age <- c()
age <- c()
for (i in 1: max(age_ordered$age)) {
  count_by_age <- append(count_by_age, nrow(filter(age_ordered, age == i)))
  AvgW_by_age <- append(AvgW_by_age, mean(filter(age_ordered, age == i)$weight))
  AvgL_by_age <- append(AvgL_by_age, mean(filter(age_ordered, age == i)$length))
  sd_by_age <- append(sd_by_age, sd(filter(age_ordered, age == i)$length))
  age <- append(age, i)
}
fish_by_age <- tibble(age,
  count = count_by_age, Avg_Weight=AvgW_by_age,Avg_length = AvgL_by_age, sd_len =sd_by_age)

ggplot(age_ordered, aes(x = age, y = length))+
  geom_point(data = fish_by_age, aes( y = Avg_length), size = 4, 
           shape = 21, fill = "red")+
  geom_smooth(method = "loess") 
# Histogram:
ggplot(fish_by_age, aes(x=age,y=Avg_length)) + geom_bar(position="dodge", stat="identity",
        fill=rainbow(15),col="black")+
  labs(title="Length of fish by age")
# Box plot:
ggplot(oo, aes(x=as.factor(fish_age),y=fish_length)) +
  geom_boxplot(fill=rainbow(14),col="black")+
  labs(y="length", x="age", title="Length of fish by age")

rm(count_by_age,AvgW_by_age,AvgL_by_age,sd_by_age,age,i)

# d)
#Creates a dataframe with 100 random fish from two random quadrants
set.seed(0601)
samp <- sample(c("NE","NW","SW","SE"),2)
q1 = filter(oo, quadrant == samp[1])
q2 = filter(oo, quadrant == samp[2])
Stringify <- paste0("The chosen quadrants are ",samp[1] , " and ", samp[2])
print(Stringify)
rm(samp,Stringify)
#takes 50 random values from the temp dataframes
set.seed(0601)
qu1 = sample_n(q1 ,50)
set.seed(0601)
qu2 = sample_n(q2 ,50)

# The next two data frames will be used in part i,j and k later
rand_quadrant <- q1
rand_quadrant_50 <- qu1
set.seed(1009)
if (sample(c(1,2),1)==1) { # letting the system choose for us one of the quadrant to be used
  rand_quadrant_50 <- qu2
  rand_quadrant <- q2
}

# e)
area1 <- qu1$fish_length
area2 <- qu2$fish_length

result = t.test(area1, area2, paired = TRUE)

combineLength <- c(q1,q2)$fish_length # to be used later
rm(area1,area2) # neccessary data kept, anything else removed 

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
# Buum til normaldreifd gogn fyrir hvert hafsvaedi med thvi ad
#   beita fallinu "get_normal_density" a lengdarmaelingar sem tilheyra
#   hverju hafsvaedi fyrir sig
normaldens <-
  oo %>%
  group_by(quadrant) %>%
  do(get_normal_density(x=.$fish_length, binwidth=3))

ggplot() + geom_histogram(data=oo_long, aes(x=fish_length),binwidth=3)+
  geom_line(data=normaldens,mapping=aes(x=fish_length,y=normal_curve),col="red",size=1)+
  facet_wrap(~quadrant)+
  labs(x="length", title="count of fish by length")

rm(get_normal_density,oo_long,normaldens)


# g lið
result <- abs(result$statistic)
set.seed(0601)
tTest <- replicate(n = 5000, t.test(sample(combineLength, 50),
                                sample(combineLength, 50),
                                paired = TRUE)$statistic,
                                simplify = TRUE )
a <- c()
for (i in 1:length(tTest)){
  if (abs(tTest[i]) > result)
    a[i] <- 1
  else
    a[i] <- 0
}
a <- sum(a) 
## Teacher's approach, probably better to use, NOTE: result is different
xyind <-c(rep(1,50),rep(2,50))
Repl <- 5000
set.seed(0601)
tTest <- sum(
  replicate(
    Repl,
    result < abs( t.test(combineLength[sample(1:length(combineLength),100)] ~ xyind )$statistic )
  )
)/Repl
rm(Repl,tTest,xyind,combineLength, a ,i )


#¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬
#h)

p2 = nrow(filter(q2, breeding_age == TRUE))/nrow(filter(oo))
res <- prop.test(x = nrow(filter(q1, breeding_age == TRUE)),
                 n = nrow(filter(q1)),
                 p = p2, correct = FALSE)


#¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬
#i)
ggplot(rand_quadrant_50, aes(x = fish_length, y = fish_mass/1000))+
  geom_point(data = rand_quadrant_50, aes( y = fish_mass/1000), size = 1, 
             shape = 21, fill = "red")+
  geom_smooth(method = "loess")  +
  labs(x="length (cm)",y="Weight (Kg)",
       title = paste0("Fish weight by length in the ",rand_quadrant_50$quadrant[1] , " area"))

ggplot(rand_quadrant_50, aes(x = log(fish_length), y = log(fish_mass)))+
  geom_point(data = rand_quadrant_50, aes( y = log(fish_mass)), size = 1, 
             shape = 21, fill = "red")+
  geom_smooth(method = "loess")  +
  labs(x="ln(length, cm)",y="ln(weight, g)",
       title=paste0("Natural log of weight by length in the ",rand_quadrant_50$quadrant[1] , " area"))

formula <- lm(log(rand_quadrant_50$fish_mass) ~ log(rand_quadrant_50$fish_length))
q <- tibble(x=log(rand_quadrant_50$fish_length), y=log(rand_quadrant_50$fish_mass))
m <- formula$coefficients[2]
a <- formula$coefficients[1]
#l <- rand_quadrant_50$fish_length[25]

# formula for predicting a fish weight:
fit <-function(l) # Mass as a function of length
{
  x <- log(l)
  ln_mass <- a + m*x
  exp(ln_mass)
}
#_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-

# j part
# create a data frame of original fish length and calculated fish mass to compare to original fish mass
len <- rand_quadrant_50$fish_length
calc_Mass <- fit(len)
compare <- tibble(x=len, y=calc_Mass)

# plotting, expected to look similar to the first graph of part i
ggplot(compare, aes(x = x, y = y/1000))+
  geom_point(data = rand_quadrant_50, aes(x=fish_length, y = fish_mass/1000), size = 1.5, 
             shape = 21, fill = "green")+
  geom_smooth(method = "loess", colour="red")  + theme_light()+
  labs(x="length (cm)",y="weight (Kg)",title="Estimated fish weight by length",
       subtitle = "point are the original weight, red is the estimated weight")+
#  stat_smooth(method='lm', se=FALSE)+
stat_smooth(data = rand_quadrant_50, aes(x=fish_length, y = fish_mass/1000),method='lm', se=FALSE)

# plotting natural log of estimated weight as a function of ln(length)
ggplot(compare, aes(x = log(x), y = log(y)))+
  geom_point(data = rand_quadrant_50, aes(x=log(fish_length), y = log(fish_mass)), size = 1.5, 
             shape = 21, fill = "red")+
  geom_smooth(method = "lm", se=FALSE) + # same line as the line the teacher wants us to use
  labs(x="length (cm)",y="weight (Kg)",title="Estimated fish weight by length",
       subtitle = "point are the original weight, red is the estimated weight")
#  stat_smooth(method='lm', se=FALSE, col="white")

#`','`','`','`','`','`','`','`','`','`','`','`','`','`','`','`','`','`','`','`','`','`','
# k part

litid <- lm(fish_length~fish_age, data=rand_quadrant)
stort <- lm(fish_length~factor(fish_age), data=rand_quadrant)
anova(litid, stort)

ggplot(rand_quadrant, aes(x=as.factor(fish_age),y=fish_length)) +
  geom_boxplot( colour = "#1418ff",fill=rainbow(11)) + labs(y="length", x="age",
  title=paste0("Length of all fish by age in the ",rand_quadrant$quadrant[1] , " area"))


# /^\,/^\,/^\,/^\,/^\,/^\,/^\,/^\,/^\,/^\,/^\,/^\,/^\,/^\,/^\,/^\,/^\,/^\,/^\,/^\,/^\,/^\
#¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬¬

# Bonus question
# this code suppose to knit plots with higher resolution but it doesn't work perfectly
#```{r setup, include=FALSE}
#knitr::opts_chunk$set(dpi=400,fig.width=5)
#```
library("sf")
library("rnaturalearth")##
library("rnaturalearthdata")##

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

reitir<-unique(oo$area)
x<-r2d(reitir)$lon
y<-r2d(reitir)$lat
sites <- data.frame(reitir,longtitude=x, latitude = y)
compass <- tibble(name=c("NE","NW","SW","SE"),longtitude=c(-12,-27,-27,-12),latitude=c(67.3,67.3,63.4,63.4))
## Icelandic map with fish coordinates (need to beautify, text overlaps)
ggplot(data = world) +
  geom_sf(color = "black", fill = "#e0e0e0")+ # Black border and blue filling
  geom_point(data = sites, aes(x = longtitude, y = latitude), size = 1, 
             shape = 24, fill = "yellow") + # Trinangular shaped yellow marks
  labs(x="longtitude", y="Latitude",title="Iceland's fishing water",
       subtitle = paste0("(", nrow(sites), " fishing spots)"))+
  coord_sf(xlim = c(min(oo$long), max(oo$long)), ylim = c(min(oo$lat),max(oo$lat)), expand = TRUE)+
  geom_text(data=sites,aes(x=longtitude,y=latitude,
            label=reitir),
            #label= paste0("(", abs(round(x, digits = 1)),",",round(y, digits = 1),")")),
            hjust=0, vjust=0, size=3,angle=30, fontface="bold",
            colour="#121212",nudge_y=-0.12,nudge_x=-0.7)+
  geom_text(data = compass, aes(x=longtitude,y=latitude,label=name),colour="red",size=5)+
  theme(panel.background = element_rect(fill = "#31b3eb",
                                        colour = "lightblue",
                                        size = 0.5, linetype = "solid"))+
  geom_vline(xintercept = -19, col="red",linetype = 'dashed')+
  geom_hline(yintercept = 65, col="red",linetype = 'dashed')+
ggsave("figure2b.jpg", dpi=150, dev='png', height=8, width=10, units="in")

rm(reitir, x, y, world, sites)
