#Sankey with only crop type, intervention and mit/adapt/both --- study counted once... 
#sankey diagram
install.packages("networkD3")
library(networkD3)

install.packages("webshot")
library(webshot)
webshot::install_phantomjs() 

#country data set: country, crop, intervention, outcome, mitigation/adaptation 
#country [location.country], crop [primary crop of focus], intervention [intervention$intervention_count], mitigation/adapt [mitigation/adaptation/both$intervention count]

# Step 1: Define helper functions and intervention groupings
clean_intervention_names <- function(x) {
  gsub("\\.", " ", x)
}

intervention_groups <- c(
  "tillage management" = "CM",
  "cover crops" = "CM",
  "crop rotation" = "CM",
  "weed management" = "CM",
  "organic fertiliser management" = "S",
  "chemical fertiliser management" = "S",
  "biochar application" = "S",
  "biostimulant" = "S",
  "inorganic amendment" = "S",
  "nitrogen inhibitor" = "S",
  "symbiotic amendment (fungi/bacteria)" = "S",
  "soil cover: residue management, mulch" = "S",
  "superabsorbent polymer" = "S",
  "solarised soil" = "S",
  "irrigation management" = "W",
  "hydraulic arrangement" = "W",
  "crop variety selection" = "CEH",
  "cultivar selection/selective breeding" = "CEH",
  "sowing timing" = "CEH",
  "planting density" = "CEH",
  "planting technology" = "CEH",
  "harvesting time" = "CEH",
  "harvesting frequency" = "CEH",
  "intercropping" = "CM",
  "agroforestry" = "CM",
  "hedgerow application" = "CM",
  "livestock integration" = "CM"
)

# Step 2: Flatten crop-country data per article
clean_intervention_names <- function(x) {
  gsub("\\.", " ", x)
}

intervention_groups <- c(
  "tillage management" = "CM",
  "cover crops" = "CM",
  "crop rotation" = "CM",
  "weed management" = "CM",
  "organic fertiliser management" = "S",
  "chemical fertiliser management" = "S",
  "biochar application" = "S",
  "biostimulant" = "S",
  "inorganic amendment" = "S",
  "nitrogen inhibitor" = "S",
  "symbiotic amendment (fungi/bacteria)" = "S",
  "soil cover: residue management, mulch" = "S",
  "superabsorbent polymer" = "S",
  "solarised soil" = "S",
  "irrigation management" = "W",
  "hydraulic arrangement" = "W",
  "crop variety selection" = "CEH",
  "cultivar selection/selective breeding" = "CEH",
  "sowing timing" = "CEH",
  "planting density" = "CEH",
  "planting technology" = "CEH",
  "harvesting time" = "CEH",
  "harvesting frequency" = "CEH",
  "intercropping" = "CM",
  "agroforestry" = "CM",
  "hedgerow application" = "CM",
  "livestock integration" = "CM"
)

# Step 2: Flatten crop-country data per article
crop_country_df <- database %>%
  filter(!is.na(primary.crop.of.focus), !is.na(location.country)) %>%
  mutate(
    location.country = str_replace_all(location.country, "South Africa", "South_Africa"),
    location.country = str_replace_all(location.country, "United States", "United_States"),
    location.country = str_replace_all(location.country, "Spain \\(not stated\\)", "Spain"),
    location.country = str_replace_all(location.country, "Spain Palestinian territories", "Spain, Palestinian territories"),
    location.country = str_replace_all(location.country, "\\(not stated\\)", "(not stated)"),
    location.country = str_replace_all(location.country, "Palestinian territories", "Palestinian_territories"),
    location.country = str_replace_all(location.country, "Global Mediterranean", "Global_Mediterranean"),
    location.country = str_replace_all(location.country, "\\b([A-Z][a-z]+) ([A-Z][a-z]+)\\b", "\\1, \\2"),
    matched_crops = map(primary.crop.of.focus, function(text) {
      text_lower <- tolower(text)
      found <- target_crops_clean[
        str_detect(text_lower, regex(tolower(target_crops_clean), ignore_case = TRUE))
      ]
      if (length(found) == 0) NA_character_ else found
    })
  ) %>%
  unnest(matched_crops, keep_empty = FALSE) %>%
  filter(!is.na(matched_crops)) %>%
  separate_rows(location.country, sep = ",|;| and | & |\\s(?=[A-Z])") %>%
  mutate(
    location.country = str_trim(str_replace_all(location.country, "_", " ")),
    crop = str_to_title(matched_crops)
  ) %>%
  filter(location.country != "") %>%
  distinct(article.ID, location.country, crop)%>%
  left_join(crop_data %>% select(crop, crop_Type), by = "crop")  


# Step 3: Flatten intervention data per article
long_intervention_df <- database %>%
  select(article.ID, all_of(intervention_cols), climate_focus) %>%
  pivot_longer(cols = all_of(intervention_cols), names_to = "intervention", values_to = "Studied") %>%
  filter(Studied == "Y") %>%
  mutate(
    intervention = clean_intervention_names(intervention),
    Group = intervention_groups[intervention],
    type = case_when(
      climate_focus == "Mitigation" ~ "Mitigation",
      climate_focus == "Adaptation" ~ "Adaptation",
      climate_focus == "Both" ~ "Both",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(type)) %>%
  select(article.ID, intervention, type)

# ??? Final joined and expanded dataset
sankey_data <- crop_country_df %>%
  inner_join(long_intervention_df, by = "article.ID", relationship = "many-to-many") %>%
  mutate(
    climate_focus = type
  ) %>%
  select(article.ID, country = location.country, crop, crop_Type, intervention, climate_focus)

head(sankey_data)

# Aggregate sankey_data at article.ID level to avoid double counting
sankey_data_aggregated <- sankey_data %>%
  group_by(article.ID, country, crop_Type, climate_focus) %>%
  summarise(
    interventions = paste(unique(intervention), collapse = ";"),
    .groups = "drop"
  ) %>%
  separate_rows(interventions, sep = ";") %>%  # explode interventions into rows
  rename(intervention = interventions) %>%
  mutate(intervention = str_trim(intervention)) %>%
  filter(intervention != "None", !is.na(intervention))

# Step 2: Create nodes (unique names from each column)
nodes <- sankey_data_aggregated %>%
  select(country, crop_Type, intervention, climate_focus) %>%
  pivot_longer(everything(), names_to = "level", values_to = "name") %>%
  distinct(name) %>%
  filter(name != "None" & !is.na(name)) %>%
  mutate(
    id = row_number() - 1,
    level = case_when(
      name %in% sankey_data_aggregated$country ~ "Country",
      name %in% sankey_data_aggregated$crop_Type ~ "Crop Type",
      name %in% sankey_data_aggregated$intervention ~ "Intervention",
      name %in% sankey_data_aggregated$climate_focus ~ "Climate Focus",
      TRUE ~ "Other"
    ),
    color = case_when(
      level == "Country" ~ "#1f77b4",
      level == "Crop Type" ~ "#ff7f0e",
      level == "Intervention" ~ "#2ca02c",
      level == "Climate Focus" ~ "#d62728",
      TRUE ~ "#7f7f7f"
    )
  )

# Step 3: Helper function to get links between levels
get_links <- function(from_col, to_col) {
  sankey_data_aggregated %>%
    count(!!sym(from_col), !!sym(to_col), name = "value") %>%
    filter(
      !!sym(from_col) != "None",
      !!sym(to_col) != "None",
      !is.na(!!sym(from_col)),
      !is.na(!!sym(to_col))
    ) %>%
    left_join(nodes, by = setNames("name", from_col)) %>%
    rename(source = id) %>%
    left_join(nodes, by = setNames("name", to_col)) %>%
    rename(target = id) %>%
    select(source, target, value) %>%
    filter(!is.na(source), !is.na(target), value > 0)
}

# Step 4: Create all links
links_country_crop <- get_links("country", "crop_Type")
links_crop_intervention <- get_links("crop_Type", "intervention")
links_intervention_focus <- get_links("intervention", "climate_focus")

all_links <- bind_rows(
  links_country_crop,
  links_crop_intervention,
  links_intervention_focus
)

# Step 5: Plot sankey network
sankey_plot_full <- sankeyNetwork(
  Links = all_links,
  Nodes = nodes,
  Source = "source",
  Target = "target",
  Value = "value",
  NodeID = "name",
  fontSize = 12,
  nodeWidth = 20,
  nodePadding = 15,
  sinksRight = FALSE
)

# Save and view
htmlwidgets::saveWidget(sankey_plot_full, "sankey_adjusted.html")
browseURL("sankey_adjusted.html")

webshot::webshot(
  url = "sankey_adjusted.html", 
  file = "sankey_diagram.png",
  vwidth = 1200,   # Width in pixels (adjust as needed)
  vheight = 800,   # Height in pixels
  zoom = 2         # Doubles resolution (set to 3-4 for ultra-high quality)
)

##########################################################################
## check counts###############
# Aggregate sankey_data at article.ID level with exploded interventions
sankey_data_aggregated <- sankey_data %>%
  group_by(article.ID, country, crop_Type, climate_focus) %>%
  summarise(
    interventions = paste(unique(intervention), collapse = ";"),
    .groups = "drop"
  ) %>%
  separate_rows(interventions, sep = ";") %>%  # explode interventions into rows
  rename(intervention = interventions) %>%
  mutate(intervention = str_trim(intervention)) %>%
  filter(intervention != "None", !is.na(intervention))

# Debug: Check aggregated data
print("Dimensions of sankey_data_aggregated:")
print(dim(sankey_data_aggregated))
print("Head of sankey_data_aggregated:")
print(head(sankey_data_aggregated))

# Count unique countries
country_counts <- sankey_data_aggregated %>%
  distinct(country) %>%
  summarise(total_countries = n())
print("Total Unique Countries:")
print(country_counts)

# Count unique interventions (as before)
intervention_counts <- sankey_data_aggregated %>%
  distinct(intervention) %>%
  summarise(total_interventions = n())
print("Total Unique Interventions:")
print(intervention_counts)

# Count studies per climate focus
climate_focus_counts <- sankey_data_aggregated %>%
  distinct(article.ID, climate_focus) %>%  # Ensure unique studies per climate focus
  count(climate_focus) %>%
  filter(climate_focus %in% c("Adaptation", "Both", "Mitigation"))
print("Climate Focus Counts (Unique Studies):")
print(climate_focus_counts)