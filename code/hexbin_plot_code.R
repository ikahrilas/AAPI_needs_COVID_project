# map plot
## load packages
library(tidyverse)
library(here)
library(geojsonio)
library(RColorBrewer)
library(rgdal)
library(sf)
library(usmap)
library(maps)
library(mapdata)
library(plotly)
library(mapproj)
library(zipcodeR)
library(socviz)
library(usdata)
library(ggthemes)
library(RColorBrewer)
library(rgdal)
library(broom)
library(rgeos)
# Load data
load(here("Output", "KeyDataset.4.18.21.RData"))

## AA data
AA_key_dataset <- key_dataset %>% filter(Race_Asian == "Yes") %>% tibble()
## NHPI data
NHPI_key_dataset <- key_dataset %>% filter(Race_HawaiianPI == "Yes") %>% tibble()

# derive density variables
AA_density <- 
  AA_key_dataset %>% 
  mutate(Zip = if_else(str_length(Zip) == 4, paste0("0", Zip), Zip)) %>% # NOTE: if zip code is 4 digits, add
  summarise(id = reverse_zipcode(Zip)$state,                             # 0 to the beginning
            county = reverse_zipcode(Zip)$county) %>% # obtain county/state data from zip code
  drop_na(id) %>% 
  group_by(id) %>% 
  summarise(total = n()) %>% 
  mutate(perc = (total / sum(total)) * 100) %>% 
  mutate(id = tolower(abbr2state(id)))

NHPI_density <- 
  NHPI_key_dataset %>% 
  mutate(Zip = if_else(str_length(Zip) == 4, paste0("0", Zip), Zip)) %>% 
  summarise(id = reverse_zipcode(Zip)$state,
            county = reverse_zipcode(Zip)$county) %>% 
  drop_na(id) %>% 
  group_by(id) %>% 
  summarise(total = n()) %>% 
  drop_na() %>% 
  mutate(perc = (total / sum(total)) * 100) %>% 
  mutate(id = tolower(abbr2state(id)))

#### HEXBIN PLOTS ####
# Download the Hexagones boundaries at geojson format here: https://team.carto.com/u/andrew/tables/andrew.us_states_hexgrid/public/map.
spdf <- geojson_read("Data/us_states_hexgrid.geojson",  what = "sp")

# reformatting
spdf@data <- spdf@data %>%
  mutate(google_name = gsub(" \\(United States\\)", "", google_name))

spdf@data <- spdf@data %>% 
  mutate(google_name = gsub(" \\(United States\\)", "", google_name))

spdf_fortified <- tidy(spdf, region = "google_name") %>% 
  mutate(id = tolower(id))

# Calculate the centroid of each hexagon to add the label:
centers <- cbind.data.frame(data.frame(gCentroid(spdf, byid = TRUE), 
                                       id = spdf@data$iso3166_2))

# hexbin map with labels
ggplot() +
  geom_polygon(data = spdf_fortified, aes( x = long, y = lat, group = group), fill = "skyblue", color = "white") +
  geom_text(data = centers, aes(x = x, y = y, label = id)) +
  theme_void() +
  coord_map()

# make hexbin map with percentage variable
states_aa <- spdf_fortified %>% 
  full_join(AA_density, by = "id") %>% 
  replace_na(list(perc = 0))

states_nhpi <- spdf_fortified %>% 
  full_join(NHPI_density, by = "id") %>% 
  replace_na(list(perc = 0))

# bin percentile variable
states_aa$bin <- cut(states_aa$perc, 
                     breaks = c(0.0, 0.17, 0.51, 1.38, 2.01, 15, 29.1), 
                     labels = c("0.0% - 0.2%", 
                              "0.2% - 0.5%", 
                              "0.5% - 1.4%", 
                              "1.4% - 2.0%",
                              "2.0% - 15.0%", 
                              "15.0% - 29.1%" ), 
                     include.lowest = TRUE)

states_nhpi$bin <- cut(states_nhpi$perc, 
                     breaks = c(0.0, 0.47, 0.94, 3.29, 10.0, 20.0, 34.12), 
                     labels=c("0.0% - 0.5%", 
                              "0.5% - 0.9%",
                              "0.9% - 3.3%", 
                              "3.3% - 10.0%", 
                              "10.0% - 20.0%",
                              "20.0% - 34.1%" ), 
                     include.lowest = TRUE)

my_palette <- c("#EBDDC4",
                "#D6B062",
                "#DB804D",
                "#95B6D1",
                "#989C78",
                "#765F56")

## from Anne:
# brown #765F56	
# beige #EBDDC4
# blue #95B6D1
# orange #DB804D	
# green #989C78	
# mustard #D6B062

# hexbin plots
ggplot() +
  geom_polygon(data = states_aa, aes(fill = bin, x = long, y = lat, group = group) , size = 0, alpha = 0.9) +
  geom_text(data = centers, aes(x = x, y = y, label = id), color = "black", size = 3, alpha = 0.6) +
  theme_void() +
  scale_fill_manual( 
    values = my_palette, 
    name = "Percentile Ranges", 
    guide = guide_legend(keyheight = unit(3, units = "mm"), keywidth = unit(12, units = "mm"), label.position = "bottom", title.position = 'top', nrow = 1) 
  ) +
  ggtitle("Geographic Distribution of AA Sample") +
  coord_map() +
  theme(
    legend.position = c(0.5, 0.9),
    text = element_text(color = "#22211d"),
    plot.title = element_text(size = 22, hjust = 0.5, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
    legend.title = element_text(hjust = 0.5)
    )
ggsave(here("AA_sample_hexbin.png"), device = "png", dpi = "retina", width = 8, height = 5)

ggplot() +
  geom_polygon(data = states_nhpi, aes(fill = bin, x = long, y = lat, group = group) , size = 0, alpha = 0.9) +
  geom_text(data = centers, aes(x = x, y = y, label = id), color = "black", size = 3, alpha = 0.6) +
  theme_void() +
  scale_fill_manual( 
    values = my_palette, 
    name = "Percentile Ranges", 
    guide = guide_legend(keyheight = unit(3, units = "mm"), keywidth = unit(12, units = "mm"), label.position = "bottom", title.position = 'top', nrow = 1) 
  ) +
  ggtitle("Geographic Distribution of NHPI Sample") +
  coord_map() +
  theme(
    legend.position = c(0.5, 0.9),
    text = element_text(color = "#22211d"),
    plot.title = element_text(size = 22, hjust = 0.5, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
    legend.title = element_text(hjust = 0.5)
  )

ggsave(here("NHPI_sample_hexbin.png"), device = "png", dpi = "retina", width = 8, height = 5)
