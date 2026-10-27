# Import relevant packages
library(tidyverse)
library(ggplot2)

# Load in data
combine = read.csv("COMBINE_original.csv")
View(combine)

# How many missing values in diet-related columns?
# Output: 205
sum(is.na(combine$dphy_plant))

# How many missing values in body mass column?
# Output: 230
sum(is.na(combine$adult_mass_g))

# How many missing values in metabolic rate column?
# WAIT, there is no metabolic rate column!
# (get this from other data sets)

# How many rows do we get if all NAs from desired columns are removed?
# (And if all debated species are removed?)
combine_filtered = drop_na(combine %>% filter(iucn2020_binomial != "Not recognised" |
                                              phylacine_binomial != "Not recognised") %>% 
                                       select(order,
                                              family,
                                              genus,
                                              species,
                                              adult_mass_g,
                                              dphy_invertebrate,
                                              dphy_vertebrate,
                                              dphy_plant,
                                              freshwater,
                                              marine,
                                              terrestrial_non.volant,
                                              terrestrial_volant) %>%
                                      mutate(ScientificName = paste(genus, species),
                                             .keep="unused",
                                             .after = family) %>%
                                      distinct(), dphy_plant)

# We are still interested in comparing metabolic rate, which will require
# merging the current data with other sources.

# Load in AnimalTraits data
animaltraits = read.csv("animaltraits.csv")

# Filter out non-mammals (call the new data frame "mammaltraits")
mammaltraits = animaltraits %>% filter(class == "Mammalia" &
                                         # Filter out rows where important info is missing
                                         (!is.na(body.mass) |
                                            original.body.mass != "" |
                                            !is.na(metabolic.rate) |
                                            !is.na(original.metabolic.rate))) %>%
  # Select relevant columns
  select(order,
         family,
         species,
         original.body.mass,
         original.body.mass...units,
         original.metabolic.rate,
         original.metabolic.rate...units) %>% 
  # Remove duplicates, if any
  distinct()

# There are duplicate species in the data set as it is a compilation of
# measurements from multiple individuals per species. We can group by
# species and take the average body mass and metabolic rates.
# Before that, however, we need to standardize the measurements.

# Convert all values in original.body.mass to decimal numbers
mammaltraits$original.body.mass = as.double(mammaltraits$original.body.mass)

# Standardize body mass measurements by converting any measurements in 
# "kg" to "g".
for (i in 1:nrow(mammaltraits)){
  if (mammaltraits$original.body.mass...units[i] == "kg"){
    # Multiply by 1000 to convert from kg to g
    mammaltraits$original.body.mass[i] = as.double(mammaltraits$original.body.mass[i])*1000
    # Change the unit recorded to g
    mammaltraits$original.body.mass...units[i] = "g"
  }
}

# Standardize all metabolic rate measurements to be in mL O2/hr as
# the other data sets report metabolic rate with these units
for (i in 1:nrow(mammaltraits)){
  if (mammaltraits$original.metabolic.rate...units[i] == "(O2) ml/min"){
    # Multiply by 60 to convert from minutes to hours
    mammaltraits$original.metabolic.rate[i] = mammaltraits$original.metabolic.rate[i]*60
    mammaltraits$original.metabolic.rate...units[i] = "(O2) ml/h"
  }
  else if(mammaltraits$original.metabolic.rate...units[i] %in% c("(O2) ml/g/h", "(O2) cm3/g/h", "(O2) cc/g/h")){
    # Multiply by body mass to remove grams from the unit
    mammaltraits$original.metabolic.rate[i] = mammaltraits$original.metabolic.rate[i]*mammaltraits$original.body.mass[i]
    mammaltraits$original.metabolic.rate...units[i] = "(O2) ml/h"
  }
  else if(mammaltraits$original.metabolic.rate...units[i] == "(O2) ml/kg/min"){
    # Multiply by body mass/1000 to remove kg (body mass is currently in grams)
    # Multiple by 60 to convert from minutes to hours
    mammaltraits$original.metabolic.rate[i] = 60*mammaltraits$original.body.mass[i]*mammaltraits$original.metabolic.rate[i]/1000
    mammaltraits$original.metabolic.rate...units[i] = "(O2) ml/h"
  }
  else if(mammaltraits$original.metabolic.rate...units[i] == "(O2) cc/g/min"){
    # Multiply by body mass to remove grams
    # Multiply by 60 to convert minutes to hour
    mammaltraits$original.metabolic.rate[i] = 60*mammaltraits$original.body.mass[i]*mammaltraits$original.metabolic.rate[i]
    mammaltraits$original.metabolic.rate...units[i] = "(O2) ml/h"
  }
  else if(mammaltraits$original.metabolic.rate...units[i] == "W/kg"){
    # 1 Watt ~ 179.1 (O2) ml/h (multiply)
    # Multiply by body mass/1000 to remove kg
    mammaltraits$original.metabolic.rate[i] = mammaltraits$original.body.mass[i]*mammaltraits$original.metabolic.rate[i]*179.1/1000
    mammaltraits$original.metabolic.rate...units[i] = "(O2) ml/h"
  }
  else if(mammaltraits$original.metabolic.rate...units[i] == "kcal/day"){
    # 1 kcal ~ 200 (O2) ml (multiply)
    # Divide by 24 to convert from day to hour
    mammaltraits$original.metabolic.rate[i] = 200*mammaltraits$original.metabolic.rate[i]/24
    mammaltraits$original.metabolic.rate...units[i] = "(O2) ml/h"
  }
}

# Group by species
mammaltraits = mammaltraits %>% group_by(species) %>%
  # Calculate mean body mass and metabolic rate
  summarize(mean_body_mass = mean(original.body.mass),
            mean_bmr = mean(original.metabolic.rate))

# Double-check for duplicate species
# FALSE, therefore no duplicate species
length(mammaltraits$species) != length(unique(mammaltraits$species))

# Read in PanTHERIA data set
pantheria = read.delim("pantheria.txt", header=TRUE, sep="\t") %>%
  # select relevant columns
  select(MSW05_Order,
         MSW05_Family,            
         MSW05_Binomial,          
         X5.1_AdultBodyMass_g,
         X18.1_BasalMetRate_mLO2hr) %>% 
  # remove duplicates, if any
  distinct()

# Missing values are represented as -999
# Replace them with NA
for (i in 1:ncol(pantheria)){        
  inds = which(pantheria[i] == -999) 
  if (length(inds) > 0){
    pantheria[,i] = replace(pantheria[,i], inds, rep(NA, length(inds)))
  }
}

# Rename columns to be more readable
colnames(pantheria) = c("Order","Family","ScientificName","AdultBodyMass_g","BasalMetRate_mLO2hr")

# Are there still duplicate species?
# FALSE, so no duplicates
length(pantheria$ScientificName) != length(unique(pantheria$ScientificName))

# How many species have null body mass values?
# Output: 71
sum(is.na(combine_filtered$adult_mass_g))

###
### Merging data sets
###

# Left joins will be used

# Join COMBINE and AnimalTraits data
merge_1 = left_join(combine_filtered, pantheria, by=join_by(ScientificName == ScientificName))

# Replace missing body mass values in COMBINE with values from PanTHERIA
for (i in 1:nrow(merge_1)){
  if (is.na(merge_1$adult_mass_g[i])){
    merge_1$adult_mass_g[i] = merge_1$AdultBodyMass_g[i]
  }
}

# How many missing body mass values do we have now?
# Output: 71 (no new body mass values were added, what a waste!)
sum(is.na(merge_1$adult_mass_g))

# Remove extra columns
merge_1 = merge_1 %>% select(-Order,
                             -Family,
                             -AdultBodyMass_g)

# How many missing BMR values do we have?
# Output: 5492
sum(is.na(merge_1$BasalMetRate_mLO2hr))

# Merge merge_1 with mammaltraits
merge_2 = left_join(merge_1, mammaltraits, by=join_by(ScientificName == species))

# Replace missing BMR values in merge_1 with values in mammaltraits
for (i in 1:nrow(merge_2)){
  if (is.na(merge_2$BasalMetRate_mLO2hr[i])){
    merge_2$BasalMetRate_mLO2hr[i] = merge_2$mean_bmr[i]
  }
}

# How many missing BMR values do we have?
# Output: 5478 (14 got added)
sum(is.na(merge_2$BasalMetRate_mLO2hr))

# Drop extra columns and remove NAs
merged_data = na.omit(merge_2 %>% select(-mean_body_mass,
                                         -mean_bmr))

###
### Data Normalization
###

# Comparing body mass and BMR is not as simple as plotting mass vs. BMR (see plot).
# Since the two are correlated (larger body mass means that your body uses more energy),
# any comparative analysis requires taking this trend into account.
ggplot(merged_data, aes(x=adult_mass_g, y=BasalMetRate_mLO2hr)) +
  geom_point() +
  labs(x="Body Mass (g)",
       y="BMR (mL O2/hr)",
       title="Body Mass vs. BMR") +
  theme_classic()

# To do this, you log-transform both body mass and basal metabolic rate
ggplot(merged_data, aes(x=log(adult_mass_g), y=log(BasalMetRate_mLO2hr))) +
  geom_point() +
  labs(x="log(Body Mass)",
       y="log(BMR)",
       title="Body Mass vs. BMR (post log-transformation)") +
  theme_classic()

# Create linear model of log-adjusted columns
mass_bmr_model = lm(log(BasalMetRate_mLO2hr) ~ log(adult_mass_g),
                    data=merged_data)
# Define vector containing residuals
adjusted_log_bmr = residuals(mass_bmr_model)
# Add residuals as a new column to the data frame
merged_data$adjusted_bmr = adjusted_log_bmr

# Last thing to do is to define diet categories based on the diet
# composition columns.
merged_data = merged_data %>% mutate(diet_type = case_when(dphy_plant == 100 ~ "Herbivore",
                                                           dphy_plant < 100 & dphy_plant > 0 ~ "Omnivore",
                                                           dphy_plant == 0 ~ "Carnivore"))

# Write the cleaned data set to a .csv file
write.csv(merged_data, "mammals.csv", row.names=FALSE)
