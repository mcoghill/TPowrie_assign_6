##	Download and analyze all of the historical fire incident location points 
# that fall within the Lac Du Bois Grasslands Protected Area 
# (you will be looking for the layer named “fire incident locations – historical”)

library(bcdata)
library(bcmaps)
library(terra)
library(mapview)
library(tidyverse)
library(ggplot2)
library(sf)

ldb <- bcdc_query_geodata("1130248f-f1a3-4956-8b2e-38d29d3e4af7", crs = 3005) |> 
  filter(PROTECTED_LANDS_NAME == "LAC DU BOIS GRASSLANDS PROTECTED AREA") |> 
  collect()

fire_search <- bcdc_search("fire incident locations")
View(fire_search)

fires_aoi <- bcdc_query_geodata(fire_search[[1]]$id, crs = 3005) %>% 
  filter(INTERSECTS(ldb)) %>% 
  collect()

mapview(fires_aoi)

#	Summarise this dataset to show how many fires occurred by fire year

fire_year <- fires_aoi %>%
  group_by(FIRE_YEAR) %>%
  summarize(fires_count = n())

print(fire_year)

#	Create another data summary that shows the overall breakdown of fire cause 
# in the Lac Du Bois Grasslands Protected Area.

fire_cause <- fires_aoi %>%
  group_by(FIRE_CAUSE) %>%
  summarize(fires_count = n())

print(fire_cause)

#	Generate a mapview of the fire points with colors to illustrate the fire cause.

mapview(fires_aoi, zcol = "FIRE_CAUSE")

#	Create a boxplot of the mean fires per year by fire cause, 
# i.e.: the X-axis should show the different fire causes, 
# and the Y-axis should show the mean number of fires per year. 
# Display each box using different colors. 

fire_counts <- fires_aoi %>%
  group_by(FIRE_YEAR, FIRE_CAUSE) %>%
  summarise(fires_count = n()) %>%
  ungroup()

mean_fires_per_year <- fire_counts %>%
  group_by(FIRE_CAUSE) %>%
  summarise(mean_fires = mean(fires_count))

ggplot(fire_counts, aes(x = FIRE_CAUSE, y = fires_count, fill = FIRE_CAUSE)) +
  geom_boxplot() +
  stat_boxplot(geom = "errorbar", width = 0.5, coef = 0) + 
  labs(x = "Fire Cause", y = "Number of Fires per Year") +
  theme_minimal()

# Okay1
# Okay2
# Okay3
# Okay4
# Okay5