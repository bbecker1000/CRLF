library(tidyverse)
raw_data <- read_csv("CRLF_EGG_RAWDATA.csv")
raw_data
#checking type of each column
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

total <- raw_data$NumberofEggMasses
total
sum(total != 0, na.rm = TRUE)
summary(total,na.rm = TRUE)

old_egg <-raw_data[raw_data$OldMass=="FALSE",]
old_total <- old_egg$NumberofEggMasses
sum(old_total != 0, na.rm = TRUE)
sum(old_total, na.rm = TRUE)
summary(old_total,na.rm = TRUE)
old_egg$BRDYEAR <- factor(old_egg$BRDYEAR)

ggplot(data = old_egg)+ 
  stat_summary(mapping = aes(x = BRDYEAR, y = NumberofEggMasses),
               fun = "mean",geom = "point")

ggplot(data = old_egg, mapping = aes(x = BRDYEAR, y = NumberofEggMasses)) + 
  geom_boxplot()


single <-old_egg|>
  group_by(BRDYEAR,Watershed)|>
  summarize(count = n(),
            mean_num = mean(NumberofEggMasses, na.rm = TRUE))
single
single <- single %>%
  mutate(total = count * mean_num)
single

ggplot(data = single, mapping = aes(x = BRDYEAR, y = mean_num)) +
  geom_point(aes(size = count), alpha = 1/3) +
  geom_smooth(se = FALSE)+facet_wrap(~Watershed)

ggplot(data = single, mapping = aes(x = BRDYEAR, y = total)) +
  geom_point(aes(size = count), alpha = 1/3) +
  geom_smooth(se = FALSE)+facet_wrap(~Watershed)


newtest <-old_egg|>
  group_by(NumberofEggMasses)|>
  summarize(count= n())
newtest

ggplot(data = old_egg, mapping = aes(x = NumberofEggMasses)) + 
  geom_freqpoly(mapping = aes(colour = BRDYEAR),bins = 4)+
  scale_x_continuous(limits = c(0, 3))

aaa

