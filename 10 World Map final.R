install.packages(c("ggplot2", "sf", "rnaturalearth", "rnaturalearthdata", "ggrepel", "terra"))
library(sf)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggrepel)
library(terra)

##latest, Grok 3 aug 1715
kg_raster <- rast("C:/Users/kent1/Desktop/PhD/Sys Lit Rev/Systematic Map/1991_2020/koppen_geiger_0p5.tif")
med_mask <- kg_raster
values(med_mask)[!values(med_mask) %in% c(8, 9, 10)] <- NA

# 2. Convert to polygons and assign fill colors
med_poly <- med_mask %>%
  as.polygons(na.rm = TRUE) %>%
  st_as_sf() %>%
  st_transform(crs = "+proj=robin") %>%
  mutate(
    climate = as.character(koppen_geiger_0p5),
    fill = case_when(
      climate == "8"  ~ "#C8C800",  # Csa - Yellow
      climate == "9"  ~ "#C8C800",  # Csb - Olive
      climate == "10" ~ "#C8C800"  # Csc - Dark Olive
    )
  )

# 3. Load and transform world boundaries
world <- ne_countries(scale = "medium", returnclass = "sf") %>%
  st_transform(crs = "+proj=robin")

# 4. Harmonize country names for matching
country_map <- country_counts %>%
  mutate(
    name = case_when(
      location.country == "South_Africa" ~ "South Africa",
      location.country == "USA" ~ "United States of America",
      location.country == "Palestinian territories" ~ "Palestine",
      location.country == "Turkiye" ~ "Turkey",
      TRUE ~ location.country
    )
  ) %>%
  select(name, total_studies)

# 5. Join harmonized country names with spatial world map
med_sf <- world %>%
  left_join(country_map, by = "name") %>%
  filter(!is.na(total_studies)) %>%   # Only keep matched countries
  distinct(name, .keep_all = TRUE)    # Remove duplicates if any

# 6. Get centroid coordinates and label string
med_centroids <- st_centroid(st_geometry(med_sf))

med_sf_coords <- med_sf %>%
  bind_cols(st_coordinates(st_centroid(st_geometry(.)))) %>%
  mutate(
    label = paste0(abbrev, " (", total_studies, ")")
  )




# plot
ggplot() +
  geom_sf(data = world, fill = "white", color = "darkgrey", linewidth = 0.2) +
  geom_sf(data = med_poly, aes(fill = fill), color = NA, alpha = 0.8) +
  
    geom_text_repel(
    data = med_sf_coords,
    aes(x = X, y = Y, label = label),
    size = 2,
    fontface = "bold",
    color = "black",
    segment.size = 0.2,
    segment.color = "white",
    box.padding = 0.2,
    max.overlaps = Inf
  ) +
  scale_fill_identity(guide = "legend", name = "Mediterranean Climate",
                      labels = c("Csa", "Csb", "Csc"),
                      breaks = c("#FFFF99", "#B3B300", "#666633")) +
  coord_sf(crs = "+proj=robin", expand = FALSE) +
  labs(title = NULL, x = NULL, y = NULL) +
  theme_minimal(base_size = 16) +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.title = element_text(face = "bold", size = 14, hjust = 0.5),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    legend.position = "bottom"
  )

ggsave("mediterranean_climate_map.png",
       plot = last_plot(),
       width = 10,       
       height = 6,
       units = "in",
       dpi = 300
)

