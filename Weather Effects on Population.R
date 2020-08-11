
# Read the Atmospheric data csv file
raw_storm_data <- read.csv("repdata_data_StormData.csv")

# Import R packages
  library(dplyr)
  library(ggplot2)

# Address the question of most dangerous types of storms

pop_hlth <- raw_storm_data %>% #Create new dataframe of only health concern data
  select(EVTYPE,
         FATALITIES,
         INJURIES)

#Aggregate by type
Fat_Strm <- aggregate(FATALITIES ~ EVTYPE, pop_hlth, FUN = sum) 
Inj_Strm <- aggregate(INJURIES ~ EVTYPE, pop_hlth, FUN = sum)

# Create 2 new vectors and 
Fat_Strm <- Fat_Strm %>%
  rename(Num = FATALITIES) %>%
  arrange(desc(Num)) %>%
  top_n(10) %>%
  mutate(Type = "Fatality") 

Inj_Strm <- Inj_Strm %>%
  rename(Num = INJURIES) %>%
  arrange(desc(Num)) %>%
  top_n(10) %>%
  mutate(Type = "Injury")

#Join each and take top 10 most severe with injury affecting more people
affect_pop <- full_join(Fat_Strm, Inj_Strm, by = c("EVTYPE", "Num", "Type")) %>%
  filter(Num > 0)

# create graph for the data
hlth_grph <- ggplot(data = affect_pop, aes(x = EVTYPE, y = Num, fill = Type)) + 
  geom_bar(stat = "identity")