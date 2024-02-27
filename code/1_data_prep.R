library(tidyverse)
raw_data <- read_csv("CRLF_EGG_RAWDATA.csv")
raw_data
#checking type of each column
str(raw_data,na.rm = TRUE)

#many ints in Survey_MONTH are empty but the values in Date look to be full. need to get month from Date

#maybe we should have survey length, width, water depth, etc as 1 standardized value for each pond per breeding year? Not sure how we want to do that

new_egg <-raw_data[raw_data$OldMass=="FALSE",]
new_egg
egg_count_by_year <- new_egg|>
  select(BRDYEAR, OldMass)|>
  group_by(BRDYEAR)|>
  summarize(count=n())
egg_count_by_year

library(ggplot2)
ggplot(data = egg_count_by_year,aes(x=BRDYEAR,y=count))+geom_point()+geom_smooth()

egg_count_by_site <-new_egg|>
  select(Watershed,BRDYEAR,OldMass)|>
  group_by(Watershed,BRDYEAR)|>
  summarize(count=n())
site_graph <- ggplot(egg_count_by_site,aes(x=BRDYEAR,y=count))+geom_point()
site_graph+facet_wrap(~Watershed)

egg_abundance <-new_egg|>
  count(Watershed,BRDYEAR)|>
  ggplot(mapping = aes(x=Watershed,y=BRDYEAR))+
  geom_tile(mapping = aes(fill=n))
egg_abundance

