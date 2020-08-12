
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

# Find the events with the largest economic impact

prop_dmg <- raw_storm_data %>%
  select(EVTYPE,
         PROPDMG,
         PROPDMGEXP)

#Mutate to multiply by damage factors
prop_dmg <- prop_dmg %>%
  mutate(PROPDMG = case_when(
         PROPDMGEXP == "K" ~ PROPDMG * 1000,
         PROPDMGEXP == "M" ~ PROPDMG * 10^6,
         PROPDMGEXP == "B" ~ PROPDMG * 10^9,
         )) %>%
  select(-PROPDMGEXP) %>% #Take out multiplier column
  rename(amount = PROPDMG) %>% #Rename to common amount
  na.omit() # Remove any NA values

#Create data matrix of crop damages
crop_dmg <- raw_storm_data %>%
  select(EVTYPE,
         CROPDMG,
         CROPDMGEXP)

#Mutate to multiply crop damage by damage factor
crop_dmg <- crop_dmg %>%
  mutate(CROPDMG = case_when(
    CROPDMGEXP == "K" ~ CROPDMG * 1000,
    CROPDMGEXP == "M" ~ CROPDMG * 10^6,
    CROPDMGEXP == "B" ~ CROPDMG * 10^9,
  )) %>%
  select(-CROPDMGEXP) %>% #Take out multiplier column
  rename(amount = CROPDMG) %>%
  na.omit()

# aggregate sum of damage by event type
prop_dmg <- aggregate(amount ~ EVTYPE, prop_dmg, FUN = sum)
prop_dmg <- prop_dmg %>% #have type of damage column
  mutate(Type = "Prop")

crop_dmg <- aggregate(amount ~ EVTYPE, crop_dmg, FUN = sum)
crop_dmg <- crop_dmg %>% #have type of damage column
  mutate(Type = "Crop")

#find the top contributors to economic impact
prop_dmg <- prop_dmg %>%
  mutate(amount = amount / 10^6) %>% #Damage in millions
  arrange(desc(amount))

crop_dmg <- crop_dmg %>%
  mutate(amount = amount / 10^6) %>% #Damage in millions
  arrange(desc(amount))

# Create full damage dataframe
tot_dam <- full_join(prop_dmg, crop_dmg) %>%
  filter(amount > 0)

#get top 10 most catastrophic
top_bad <- aggregate(amount ~ EVTYPE, tot_dam, sum) %>% 
  arrange(desc(amount)) %>%
  top_n(10)

#Filter the top 10 most catastrophic
tot_dam <- filter(tot_dam, tot_dam$EVTYPE %in% top_bad$EVTYPE)

dmg_grph <- ggplot(data = tot_dam, aes(x = factor(EVTYPE), y = amount, fill = Type)) + 
  geom_bar(stat = "identity")
