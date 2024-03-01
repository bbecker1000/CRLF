#this is where all of our exploratory data analysis should go, as the 1_data_prep file should only be for data import, prep, and cleaning
#hopefully this will help us make our code more readable and modular

#I also think it would be nice to have comments for each of these graphs to let everyone know what they do (and so we can see what other
#graphs need to be made!) this would just help the code be more readable and help with collaboration --Robin

source("1_data_prep.R")

#table of how many surveys were done each year
survey_count_by_year <- raw_data|>
  group_by(BRDYEAR)|>
  summarize(count=n())
survey_count_by_year

#plots survey count by year across all watersheds/sites
ggplot(data = survey_count_by_year,aes(x=BRDYEAR,y=count))+geom_point()+geom_smooth()

#table of how many surveys were done at each watershed per year
survey_count_by_shed <-raw_data|>
  group_by(Watershed,BRDYEAR)|>
  summarize(count=n())
survey_count_by_shed

#plots survey count by watershed by year
site_graph <- ggplot(survey_count_by_site,aes(x=BRDYEAR,y=count,colour=Watershed,group=Watershed))+geom_point()
site_graph
site_graph+facet_wrap(~Watershed) #separates graphs by watershed

#heatmap of suvey count per year by watershed
survey_abundance <-raw_data|>
  count(Watershed,BRDYEAR)|>
  ggplot(mapping = aes(x=Watershed,y=BRDYEAR))+
  geom_tile(mapping = aes(fill=n))
survey_abundance

#I think this gets the total number of egg masses over all years? Not quite sure, this isn't my code lol --Robin
new_egg <-raw_data[raw_data$OldMass=="FALSE",]
new_total <- new_egg$NumberofEggMasses
sum(new_total, na.rm = TRUE)
summary(new_total,na.rm = TRUE)
new_egg$BRDYEAR <- factor(new_egg$BRDYEAR)
total <- new_egg$NumberofEggMasses
sum(total,na.rm = TRUE)
sum(total != 0, na.rm = TRUE)

#plots number of egg masses recorded per year across all watersheds
ggplot(data = new_egg)+ 
  stat_summary(mapping = aes(x = BRDYEAR, y = NumberofEggMasses),
               fun = "mean",geom = "point",color = "blue", size= 2) 

#boxplot of above graph
ggplot(data = new_egg, mapping = aes(x = BRDYEAR, y = sqrt(NumberofEggMasses)))+
  geom_boxplot()

#stats for number of egg masses by watershed by year?
statistics <-new_egg|>
  group_by(BRDYEAR,Watershed)|>
  summarize(count = n(),
            mean_num = mean(NumberofEggMasses, na.rm = TRUE),
            total_num =sum(NumberofEggMasses, na.rm = TRUE)
  )
statistics

#plot of total egg masses over time per watershed
ggplot(data = statistics, mapping = aes(x = BRDYEAR, y = total_num)) +
  geom_point(aes(size = count), alpha = 1/2) + facet_wrap(~Watershed)

#plot of mean egg masses over time by watershed
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
