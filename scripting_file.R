library(tidyverse)
library(janitor)
library(osmdata)

county_list <- read_csv("kentucky_counties.csv") %>%
  clean_names() %>% 
  select(-area) %>%
  mutate(lookup = paste(county, "Kentucky"))

#county_list$lookup[1] <- "Dallas Texas"

streets <- getbb(county_list$lookup[15])%>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("motorway", "primary", 
                            "secondary", "tertiary")) %>%
  osmdata_sf()

small_streets <- getbb(county_list$lookup[15])%>%
  opq()%>%
  add_osm_feature(key = "highway",
                  value = c("residential", "living_street")) %>%
  filter()
  osmdata_sf()

river <- getbb(county_list$lookup[15])%>%
  opq()%>%
  add_osm_feature(key = "waterway", value = "river") %>%
  osmdata_sf()

limits <- getbb(county_list$lookup[15]) %>%
  as.data.frame()%>%
  rownames_to_column("coordinate") %>%
  pivot_longer(cols = c(2:3), names_to = "minmax", values_to = "value") %>%
  unite("coord", 1:2) %>% 
  t() %>% 
  as_tibble() %>% 
  row_to_names(1) %>%
  mutate_all(as.numeric)

ggplot() +
  geom_sf(data = small_streets$osm_lines,
          inherit.aes = FALSE,
          color = "grey80",
          size = .4,
          alpha = .6) +
  geom_sf(data = river$osm_lines,
          inherit.aes = FALSE,
          color = "steelblue",
          size = .2,
          alpha = .5) +
  geom_sf(data = streets$osm_lines,
          inherit.aes = FALSE,
          color = "black",
          size = .7,
          alpha = .8) +
   coord_sf(xlim = c(limits$x_min, limits$x_max), 
            ylim = c(limits$y_min, limits$y_max),
            expand = FALSE) +
  theme_void()
