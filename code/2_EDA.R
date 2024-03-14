source("1_data_prep.R")
setwd(here::here("code"))

### ~~~ *** DATA MANIPULATION (tables that are helpful for creating many graphs) *** ~~~ ###

# table for number of egg masses by year by distinct location (sites)
totalEggsPerYear <- data %>%
  group_by(Watershed, LocationID, BRDYEAR) %>%
  summarize(totalEggs = sum(NumberofEggMasses))
totalEggsPerYear

# table to denote first and last egg mass detected per year, as well as length of breeding season
eggTiming <- data %>%
  filter(NumberofEggMasses > 0, OldMass == "FALSE") %>%
  group_by(Watershed, LocationID, BRDYEAR) %>%
  summarize(firstEgg = min(dayOfWY), lastEgg = max(dayOfWY), breedingLength = max(dayOfWY) - min(dayOfWY))
eggTiming

### ~~~ *** PLOTS *** ~~~ ###

# plot total number of eggs per year by site (shortened x-axis for view)
library(ggplot2)
number_of_eggs_per_year_by_site <-
  ggplot(data = totalEggsPerYear, aes(x = BRDYEAR, y = totalEggs)) +
  geom_point() + facet_wrap(totalEggsPerYear$LocationID) + 
  scale_x_continuous(breaks = seq(min(totalEggsPerYear$BRDYEAR), max(totalEggsPerYear$BRDYEAR), by = 6))
number_of_eggs_per_year_by_site 

# plot timing of egg laying by year and watershed
eggTimingNoZero <- eggTiming %>% filter(breedingLength > 0) %>% group_by(Watershed, BRDYEAR) %>% 
  summarize(meanFirstEgg = mean(firstEgg), meanLastEgg = mean(lastEgg), meanLength = mean(lastEgg) - mean(firstEgg))
ggplot(data = eggTimingNoZero, aes(x = BRDYEAR, y = meanLength, color = Watershed)) + geom_line()

# correlate this^^ with rainfall! (TODO -- need to do rainfall data first)

#table of how many surveys were done each year (not relevant)
survey_count_by_year <- data|>
  group_by(BRDYEAR)|>
  summarize(survey_count=n_distinct(EventGUID)) #this is wrong
survey_count_by_year

#plots survey count by year across all watersheds/sites
ggplot(data = survey_count_by_year,aes(x=BRDYEAR,y=survey_count))+geom_point()+geom_smooth()

#table of number of surveys at each watershed per year
abundance_counts <- data %>%
  group_by(Watershed, BRDYEAR) %>%
  summarize(Count = n_distinct(EventGUID))
abundance_counts

#only look at watersheds with decent amount of data?
rich_watersheds <-abundance_counts|>
  filter(Watershed == "Kanoff Creek" | Watershed == "Laguna Salada" | Watershed =="Milagra Creek"|
           Watershed == "Redwood Creek" | Watershed == "Rodeo Lagoon" | Watershed=="Tennessee Valley" |
           Watershed == "Wilkins Gulch")

rich_graph <- ggplot(rich_watersheds,aes(x=BRDYEAR,y=Count,colour=Watershed,group=Watershed))+geom_point()
rich_graph <- rich_graph+ facet_wrap(~Watershed)
rich_graph

#heatmap of survey count per year by watershed
ggplot(abundance_counts, aes(x = Watershed, y = BRDYEAR, fill = Count)) +
  geom_tile() +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +  # Adjust color gradient
  labs(x = "Watershed", y = "BRDYEAR", title = "Abundance Heatmap") +  # Add axis labels and title
  theme_minimal()

#summary information of new egg masses count
new_egg <-data[data$OldMass=="FALSE",]
new_total_egg_masses <- new_egg$NumberofEggMasses
#total number of egg masses is 4000
sum(new_total_egg_masses, na.rm = TRUE)
#maximum number of egg masses for a given survey is 25, while the mean is 0.84
summary(new_total_egg_masses,na.rm = TRUE)
new_egg$BRDYEAR <- factor(new_egg$BRDYEAR)


#plots of mean number of new egg masses per survey across all watersheds through time
ggplot(data = new_egg)+ 
  stat_summary(mapping = aes(x = BRDYEAR, y = NumberofEggMasses),
               fun = "mean",geom = "point",color = "blue", size= 2)+
  labs(x = "Year", y = "mean number of new egg masses")
 
#boxplot of above graph (y-axis is square-rooted for visualization)
ggplot(data = new_egg, mapping = aes(x = BRDYEAR, y = sqrt(NumberofEggMasses)))+
  geom_boxplot()+
  labs(x = "Year", y = "sqrt of mean number of new egg masses")

#stats for 1.number of survey, 2. mean number of new egg masses per survey, and 3.
#total number of egg masses by watershed by site by year
statistics <-new_egg|>
  group_by(BRDYEAR,Watershed,LocationID)|>
  summarize(count = n_distinct(EventGUID),
            mean_num = mean(NumberofEggMasses, na.rm = TRUE),
            total_num =sum(NumberofEggMasses, na.rm = TRUE))
statistics_rich <- statistics |>
  filter(Watershed == "Kanoff Creek" | Watershed == "Laguna Salada" | Watershed =="Milagra Creek"|
                  Watershed == "Redwood Creek" | Watershed == "Rodeo Lagoon" | Watershed=="Tennessee Valley" |
                  Watershed == "Wilkins Gulch") |>
  filter(count > 1)
statistics_rich$count <- as.factor(statistics_rich$count)

#plot of total egg masses over time per site
ggplot(data = statistics_rich, mapping = aes(x = BRDYEAR, y = total_num)) +
  geom_point(aes(size = count), alpha = 1/2) + facet_wrap(~LocationID) +
  labs(x = "Year", y = "total new egg masses")
  

#plot of mean egg masses over time by site
ggplot(data = statistics, mapping = aes(x = BRDYEAR, y = mean_num)) +
  geom_point(alpha = 1/3) + facet_wrap(~LocationID)+
  labs(x = "Year", y = "mean new egg masses")

#example: showing the number of survey for all sites in Redwood Creek watershed in 2016
data|>
  filter(Watershed=="Redwood Creek")|>
  filter(BRDYEAR == "2016")|>
  group_by(Watershed,LocationID)|>
  summarize(survey_count=n_distinct(EventGUID))


#number of survey for each number of new egg masses of all watersheds by year
ggplot(data = new_egg, mapping = aes(x = NumberofEggMasses)) + 
  geom_freqpoly(mapping = aes(colour = BRDYEAR),bins = 4)+
  scale_x_continuous(limits = c(0, 3))


#table showing number of different sites for all watersheds
number_of_sites_within_watershed <- data|>
  group_by(Watershed)|>
  summarize(distinct_count = n_distinct(LocationID))
number_of_sites_within_watershed

#plot of the above table.
ggplot(number_of_sites_within_watershed,aes(x=Watershed,y=distinct_count))+
  geom_point()


#histogram of response data for all years pooled (may change x-axis, not that helful)
ggplot(data = data, aes(x = NumberofEggMasses)) + geom_histogram()

n_distinct(raw_data$EventGUID)
