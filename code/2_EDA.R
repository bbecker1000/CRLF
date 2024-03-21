library(ggplot2)
source("1_data_prep.R")
setwd(here::here("code"))

### ~~~ *** DATA MANIPULATION (tables that are helpful for creating many graphs) *** ~~~ ###

#summary information of new egg masses count
new_egg <-data[data$OldMass=="FALSE",]
new_total_egg_masses <- new_egg$NumberofEggMasses
#total number of egg masses is 4000
sum(new_total_egg_masses, na.rm = TRUE)
#maximum number of egg masses for a given survey is 25, while the mean is 0.84
summary(new_total_egg_masses,na.rm = TRUE)
new_egg$BRDYEAR <- factor(new_egg$BRDYEAR)

#stats for 1.number of survey, 2. mean number of new egg masses per survey, 3.
#total number of egg masses per watershed per site per year, 4. first and last egg mass detected per year, 
# 5. length of breeding season
statistics <-new_egg|>
  group_by(BRDYEAR,Watershed,LocationID)|>
  summarize(survey_count = n_distinct(EventGUID),
            mean_egg_num = (sum(NumberofEggMasses, na.rm = TRUE)/survey_count),
            total_egg_num =sum(NumberofEggMasses, na.rm = TRUE),
            firstEgg = min(dayOfWY), 
            lastEgg = max(dayOfWY), 
            breedingLength = max(dayOfWY) - min(dayOfWY)
            )

### ~~~ *** PLOTS *** ~~~ ###

# plot total number of eggs per year by site (shortened x-axis for view)
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
  summarize(survey_count=n_distinct(EventGUID))
survey_count_by_year

#plots survey count by year across all watersheds/sites
ggplot(data = survey_count_by_year,aes(x=BRDYEAR,y=survey_count))+geom_point()+geom_smooth()

#table of number of surveys at each watershed per year
abundance_counts <- data %>%
  group_by(Watershed, BRDYEAR) %>%
  summarize(Count = n_distinct(EventGUID))
abundance_counts

#Plot showing number of surveys per year
#only at watersheds with decent amount of data (7 watersheds in total) 
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


#plots of mean number of new egg masses per survey across all watersheds of all years
ggplot(data = statistics)+ 
  stat_summary(mapping = aes(x = BRDYEAR, y = mean_num),
               fun = "mean",geom = "point",color = "blue", size= 2)+
  labs(x = "Year", y = "mean number of new egg masses")


#above graph only looking at 7 target watersheds, and filter by having at least 2 new egg masses per year
statistics_rich <- statistics |>
  filter(Watershed == "Kanoff Creek" | Watershed == "Laguna Salada" | Watershed =="Milagra Creek"|
                  Watershed == "Redwood Creek" | Watershed == "Rodeo Lagoon" | Watershed=="Tennessee Valley" |
                  Watershed == "Wilkins Gulch") |>
  filter(count > 1)
statistics_rich$count <- as.factor(statistics_rich$count)

#plot of total egg masses over time per site at the 7 "rich"watersheds
ggplot(data = statistics_rich, mapping = aes(x = BRDYEAR, y = total_num)) +
  geom_point(aes(size = count), alpha = 1/2) + facet_wrap(~LocationID) +
  labs(x = "Year", y = "total new egg masses")
  
#plot of mean egg masses over time per site at the 7 "rich"watersheds
ggplot(data = statistics, mapping = aes(x = BRDYEAR, y = mean_num)) +
  geom_point(alpha = 1/3) + facet_wrap(~LocationID)+
  labs(x = "Year", y = "mean new egg masses")

#example: number of survey for all sites in Redwood Creek watershed in 2016
data|>
  filter(Watershed=="Redwood Creek")|>
  filter(BRDYEAR == "2016")|>
  group_by(Watershed,LocationID)|>.
  summarize(survey_count=n_distinct(EventGUID))

#number of survey count for each number of new egg masses of all watersheds by year
ggplot(data = statistics, mapping = aes(x = total_num)) + 
  geom_freqpoly(mapping = aes(colour = BRDYEAR),bins = 4)+
  scale_x_continuous(limits = c(0, 200))

#table showing number of different sites for all watersheds
number_of_sites_within_watershed <- data|>
  group_by(Watershed)|>
  summarize(distinct_count = n_distinct(LocationID))
number_of_sites_within_watershed

#plot of the above table.
ggplot(number_of_sites_within_watershed,aes(x=Watershed,y=distinct_count))+
  geom_point()

#counting the number of non-NA values for the environmental variables we are looking at 
data$WaterSalinity
non_na_Salinity <- na.omit(data$WaterSalinity)
length(non_na_Salinity)
sum(non_na_Salinity != 0)
#there are 1200 salinity data, only 92 of them is non-zero. 

data |>
  filter(data$WaterSalinity>0)|>
  group_by(Watershed)|>
  summarise(count=n())
#the distribution of these92 non-zero salinity data by watershed.

data$PercentOpenWater
non_na_open <- na.omit(data$PercentOpenWater)
length(non_na_open)
sum(non_na_open != 0)
#there are 4937 non-zero open water data

data$AvgD
non_na_depth <- na.omit(data$AvgD)
length(non_na_depth)
#there are 3404 non-zero open water data

data$MaxD
non_na_depth2 <- na.omit(data$MaxD)
length(non_na_depth2)
#there are 5091 non-zero open water data

#correlation between average depth and max depth is significantly positive 
regression_depth=lm(AvgD~MaxD,data=new_egg)
summary(regression_depth)


ggscatter(new_egg, x = "MaxD", y = "AvgD",
          add = "reg.line",cor.coef=TRUE,coor.method=" ")



# investigating number of observers (obsv_total)
## histogram of obsv_total
ggplot(data = data, aes(x = obsv_total)) + geom_histogram()


