# map plot for hate group
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

# load data
dat <- read_csv("Data/hate_group.csv", col_names = c("state", "num")) %>% 
  filter(!(state %in% c("Cyber/Internet/News", "Abroad"))) %>% 
  mutate(state = tolower(state),
         state = if_else(state == "washington d.c.", "district of columbia", state),
         state = if_else(state == "new hamsphire", "new hampshire", state),
         perc = num / sum(num) * 100)

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

# data with hate group and hexbin info
states_hate <- spdf_fortified %>% 
  full_join(dat, by = c("id" = "state")) %>% 
  replace_na(list(perc = 0))

# bin percentile variable
states_hate$bin <- cut(states_hate$perc, 
                       breaks = c(0.0, 0.35, 0.702, 1.8, 3.51, 16.2, 41), 
                       labels = c("0.0% - 0.4%", 
                                  "0.4% - 0.7%", 
                                  "0.7% - 1.8%", 
                                  "1.8% - 3.5%",
                                  "3.5% - 16.2%", 
                                  "16.2% - 40.7%" ), 
                       include.lowest = TRUE)

# colors
my_palette <- c("#EBDDC4",
                "#D6B062",
                "#DB804D",
                "#95B6D1",
                "#989C78",
                "#765F56")

# plot it
ggplot() +
  geom_polygon(data = states_hate, aes(fill = bin, x = long, y = lat, group = group) , size = 0, alpha = 0.9) +
  geom_text(data = centers, aes(x = x, y = y, label = id), color = "black", size = 3, alpha = 0.6) +
  theme_void() +
  scale_fill_manual( 
    values = my_palette, 
    name = "Percentile Ranges", 
    guide = guide_legend(keyheight = unit(3, units = "mm"), keywidth = unit(12, units = "mm"), label.position = "bottom", title.position = 'top', nrow = 1) 
  ) +
  ggtitle("Geographic Distribution of\nHate Incident Reporters") +
  coord_map() +
  theme(
    legend.position = c(0.5, 0.9),
    text = element_text(color = "#22211d"),
    plot.title = element_text(size = 22, hjust = 0.5, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
    legend.title = element_text(hjust = 0.5)
  )
# save it
ggsave(here("hate_incident_hexbin.png"), device = "png", dpi = "retina", width = 8, height = 5)
