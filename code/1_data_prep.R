library(tidyverse)
raw_data <- read_csv("CRLF_EGG_RAWDATA.csv")

raw_data
#checking type of each column
str(raw_data)
str(raw_data,na.rm = TRUE)

#many ints in Survey_MONTH are empty but the values in Date look to be full. need to get month from Date

#maybe we should have survey length, width, water depth, etc as 1 standardized value for each pond per breeding year? Not sure how we want to do that

survey_count_by_year <- raw_data|>
  group_by(BRDYEAR)|>
  summarize(count=n())
survey_count_by_year

library(ggplot2)
ggplot(data = survey_count_by_year,aes(x=BRDYEAR,y=count))+geom_point()+geom_smooth()

survey_count_by_site <-raw_data|>
  group_by(Watershed,BRDYEAR)|>
  summarize(count=n())
survey_count_by_site
site_graph <- ggplot(survey_count_by_site,aes(x=BRDYEAR,y=count,colour=Watershed,group=Watershed))+geom_point()
site_graph
site_graph+facet_wrap(~Watershed)

survey_abundance <-raw_data|>
  count(Watershed,BRDYEAR)|>
  ggplot(mapping = aes(x=Watershed,y=BRDYEAR))+
  geom_tile(mapping = aes(fill=n))
survey_abundance

new_egg <-raw_data[raw_data$OldMass=="FALSE",]
new_total <- new_egg$NumberofEggMasses
sum(new_total, na.rm = TRUE)
summary(new_total,na.rm = TRUE)
new_egg$BRDYEAR <- factor(new_egg$BRDYEAR)

total <- new_egg$NumberofEggMasses
sum(total,na.rm = TRUE)
sum(total != 0, na.rm = TRUE)

ggplot(data = new_egg)+ 
  stat_summary(mapping = aes(x = BRDYEAR, y = NumberofEggMasses),
               fun = "mean",geom = "point",color = "blue", size= 2) 
  

ggplot(data = new_egg, mapping = aes(x = BRDYEAR, y = sqrt(NumberofEggMasses)))+
  geom_boxplot()

statistics <-new_egg|>
  group_by(BRDYEAR,Watershed)|>
  summarize(count = n(),
            mean_num = mean(NumberofEggMasses, na.rm = TRUE),
            total_num =sum(NumberofEggMasses, na.rm = TRUE)
            )
statistics

ggplot(data = statistics, mapping = aes(x = BRDYEAR, y = total_num)) +
  geom_point(aes(size = count), alpha = 1/2) + facet_wrap(~Watershed)

ggplot(data = statistics, mapping = aes(x = BRDYEAR, y = mean_num)) +
  geom_point(alpha = 1/3) + facet_wrap(~Watershed)


newtest <-new_egg|>
  group_by(NumberofEggMasses)|>
  summarize(count= n())
newtest

ggplot(data = new_egg, mapping = aes(x = NumberofEggMasses)) + 
  geom_freqpoly(mapping = aes(colour = BRDYEAR),bins = 4)+
  scale_x_continuous(limits = c(0, 3))

raw_data|>
  select(Watershed,LocationID)|>
  group_by(Watershed,LocationID)|>
  summarize(count=n())|>
  filter(Watershed=="Redwood Creek")

number_of_sites_within_watershed <- raw_data|>
  group_by(Watershed)|>
  summarize(distinct_count = n_distinct(LocationID))
number_of_sites_within_watershed

ggplot(number_of_sites_within_watershed,aes(x=Watershed,y=distinct_count))+
  geom_point()
