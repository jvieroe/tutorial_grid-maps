pacman::p_load(rio,
               tidyverse,
               magrittr,
               sf,
               knitr,
               magick,
               viridisLite)

rm(list=ls())

setwd("/Users/jeppeviero/Dropbox/02 PhD/18 tutorials/grid_maps")

set_crs <- 5643 # later on we will need a certain CRS. Some doesn't allow us to make grid the same size
# This one does ... but others do too, and I'm not sure this one is ideal

# Load city data
cities <- rio::import("bairoch.Rdata") %>% 
  dplyr::select(c(city_id, year, popexp, latitude, longitude)) %>% # keep only some of the variables
  st_as_sf(coords = c("longitude", "latitude"),
           crs = 4326) %>% 
  st_transform(set_crs)

# ----- Keep only unique cities (you don't need to do this, it's only because I have a panel)
cities <- cities %>% 
  distinct(., city_id, .keep_all = T)

# ----- Load map data
# you need all the europe_map_union files in a folder called europe_map_union (referred to with 'dsn')
europe_union <- st_read(dsn = "ne_10m_admin_0_map_units",
                        layer = "ne_10m_admin_0_map_units",
                        crs = 4326)

europe_union <- europe_union %>% 
  filter(REGION_UN == "Europe") %>% 
  st_union() %>% 
  st_as_sf()

europe_union <- st_crop(europe_union,
                  st_bbox(c(xmin = -13, xmax = 32,
                            ymin = 35, ymax = 65),
                          crs = st_crs(europe_union)))

europe_union <- europe_union %>% 
  st_transform(set_crs) %>% 
  mutate(FID = 1)

# ----- Plot it
ggplot() + 
  geom_sf(data = europe_union, fill = "grey90", alpha = .5, size = 0.25) +
  geom_sf(data = cities, size = 0.25) +
  theme_classic()

# ----- Create grid (equal size cells)
grid_spacing <- 100*10^3 # define size of grids. the CRS calculates distance in metes so we want 100*1000 for 100km*100km cells

grid_sf <- europe_union %>% # create grid
  st_make_grid(.,
               cellsize = c(grid_spacing, grid_spacing),
               square = T) %>% 
  st_as_sf() %>% 
  mutate(area_km2 = unclass(st_area(.))/1000000) %>% # calculate area of each cell just to check
  mutate(length = sqrt(area_km2)) %>% # calculate length/width of each cell just to check
  mutate(grid_id = row_number()) # give each cell an ID

hist(grid_sf$area_km2) # uniform distribution of area.. the equal area thing worked! Hallelujah!
min(grid_sf$area_km2) # 100km times 100km
max(grid_sf$area_km2)

# ----- Plot the grid on top of the other map
ggplot() + 
  geom_sf(data = europe_union, fill = "grey90", alpha = .5, size = 0.25) +
  geom_sf(data = cities, size = 0.25) +
  geom_sf(data = grid_sf, fill = NA, size = 0.1) +
  theme_classic()

# ----- Match cities to grids
cities <- cities %>% # first we add a 'city dummy' that we can count later 
  mutate(city_dummy = 1)

city_grid <- st_join(grid_sf, cities) # now each city has a grid ID

city_grid <- city_grid %>% 
  arrange(grid_id)

any(duplicated(city_grid$grid_id)) # grid cells are duplicated at the city level. Let's summarize at the grid cell level

# ----- Summarize city count at the grid cell level
sum_grid <- city_grid %>% 
  group_by(grid_id) %>% 
  dplyr::summarize(city_count = sum(city_dummy, na.rm = T)) # sum the dummy we made before

# ----- Plot city count
ggplot() + 
  geom_sf(data = sum_grid, aes(fill = city_count), size = 0.1) +
  theme_classic()

# That doesn't look too nice. Let's look at the distribution of city_count
hist(sum_grid$city_count)

# Very skewed! Let's try a log transformation
sum_grid <- sum_grid %>% 
  mutate(ln_city_count = log(city_count + 1))

# ----- Plot it
ggplot() + 
  geom_sf(data = sum_grid, aes(fill = ln_city_count), size = 0.1) +
  theme_classic()

# That's a little better. But, all the cells with no dryland are just confusing us! Let's add info on that
sum_grid <- st_join(sum_grid, europe_union)

# the only column in the 'europe_union' data (FID) has now been added to the grid cells
# That means that cells with NA in the FID column are maritime
# No need to show them! Besides Atlantis, there are no maritime cities, after all

sum_grid <- sum_grid %>% 
  # this says: if FID is NOT missing (NA), keep the value of city_count. Otherwise, replace with NA
  mutate(city_count_dryland = ifelse(!is.na(FID), city_count, NA)) %>%
  # log transform the one we just made
  mutate(ln_city_count_dryland = log(city_count_dryland + 1))

# ----- Plot it (log)
ggplot() + 
  geom_sf(data = sum_grid, aes(fill = ln_city_count_dryland), size = 0.1) +
  theme_classic()

# Well, we're getting there! We can recognize the shape of Europe, and the city density matches what we know
# But, it's not super pretty. It's a very chunky map. Try to:
# (1) change the 'grid_spacing' parameter to, e.g., 50km (50*10^3)
# (2) run the whole thing again,
# (3) enjoy!

# That's MUCH better! But, something comes to mind
# .... Why does the Mediterranean Sea and Atlantic Ocean have to be an ugly grey color? Let's fix that!

# ----- Plot it (log)
ggplot() + 
  geom_sf(data = sum_grid, aes(fill = ln_city_count_dryland), size = 0.1) +
  scale_fill_continuous(na.value = "white") +
  theme_classic()

# Way to go! But, now the black lines defining the grid cells are hard to ignore. Another fix!

# ----- Plot it (log)
ggplot() + 
  geom_sf(data = sum_grid, aes(fill = ln_city_count_dryland), color = NA, size = 0.1) +
  scale_fill_continuous(na.value = "white") +
  theme_classic()

# Finally, a few aesthetics that could be better

# ----- Plot it (log)
ggplot() + 
  geom_sf(data = sum_grid, aes(fill = ln_city_count_dryland), color = NA, size = 0.1) +
  scale_fill_continuous(na.value = "white") +
  theme_classic() +
  scale_x_continuous(breaks = seq(-5, 30, 10)) + # scale the longitude axis labels
  theme(legend.position = "bottom") + # move legend to bottom
  guides(fill = guide_colourbar(title = "City count (log)")) # rename our legend


# Try some other color palettes! 
# ... Here are three palettes from the viridisLite package, but there's a million palettes out there!

# ----- Plot it (log)

ggplot() + 
  geom_sf(data = sum_grid, aes(fill = ln_city_count_dryland), color = NA, size = 0.1) +
  scale_fill_viridis_c(option = "viridis", na.value = "white") +
  theme_classic() +
  scale_x_continuous(breaks = seq(-5, 30, 10)) + 
  theme(legend.position = "bottom") + 
  guides(fill = guide_colourbar(title = "City count (log)"))

ggplot() + 
  geom_sf(data = sum_grid, aes(fill = ln_city_count_dryland), color = NA, size = 0.1) +
  scale_fill_viridis_c(option = "plasma", na.value = "white") +
  theme_classic() +
  scale_x_continuous(breaks = seq(-5, 30, 10)) + 
  theme(legend.position = "bottom") + 
  guides(fill = guide_colourbar(title = "City count (log)"))

ggplot() + 
  geom_sf(data = sum_grid, aes(fill = ln_city_count_dryland), color = NA, size = 0.1) +
  scale_fill_viridis_c(option = "inferno", na.value = "white") +
  theme_classic() +
  scale_x_continuous(breaks = seq(-5, 30, 10)) + 
  theme(legend.position = "bottom") + 
  guides(fill = guide_colourbar(title = "City count (log)"))


# ----- Save your pretty map! You don't want it to be for yours eyes only, right? Share it with the world!
getwd()
ggsave(plot = last_plot(), # save map
       file = "grid_map.png") # choose where you want it
knitr::plot_crop("grid_map.png", # crop map to avoid white space
                 quiet = T)
