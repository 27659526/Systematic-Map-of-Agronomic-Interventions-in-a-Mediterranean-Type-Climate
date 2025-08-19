
# ==== 01 load.data_database.R ====
library(readxl)

#Download the Supplementary material and upload sheet "Database"
database <- read_excel(
  "[yourpath]/Supplementary Material_agr_intervention_database.xlsx",
  sheet = "Database",
  skip = 1
)

# ==== 02 publication_country.counts.R ====
library(dplyr)
library(stringr)
library(tidyr)

##################################################################

#   1. Count of studies by adaptation/mitigation/both over time  #

##################################################################

# Classify article type
database <- database %>%
 mutate(
  mitigation.finding = if_else(is.na(mitigation.finding) | str_trim(mitigation.finding)
    == "", "N/A", mitigation.finding),
  adaptation.finding = if_else(is.na(adaptation.finding) | str_trim(adaptation.finding)
    == "", "N/A", adaptation.finding)
  ) %>%
  mutate(
    climate_focus = case_when(
      mitigation.finding == "N/A" & adaptation.finding != "N/A" ~ "Adaptation",
      adaptation.finding == "N/A" & mitigation.finding != "N/A" ~ "Mitigation",
      adaptation.finding != "N/A" & mitigation.finding != "N/A" ~ "Both",
      TRUE ~ "Unclear"
    )
  )

# Count articles per year of publication and type
pub_year_summary <- database %>%
  group_by(year.of.publication, climate_focus) %>%
  summarise(n = n(), .groups = "drop") %>%
  complete(year.of.publication, climate_focus, fill = list(n = 0)) %>%
  group_by(year.of.publication) %>%
  mutate(
    year_total = sum(n),
    year_mean = mean(n),
    year_median = median(n)
  ) %>%
  ungroup()

##################################################################

#   2. Meta-data Analysis of studies: count data  #

##################################################################

# Calculate the median samples size of publications over time
year_totals <- pub_year_summary %>%
  group_by(year.of.publication) %>%
  summarise(year_total = unique(year_total)) %>%
  arrange(year_total)

# Find the median value of publications overtime
n_years <- nrow(year_totals)
median_index <- ceiling(n_years / 2)
median_value <- median(year_totals$year_total)
median_year <- year_totals$year.of.publication[median_index]

# Print result
cat("Median number of studies per year:", median_value, "\n")
cat("Year closest to that median value:", median_year, "\n")

# Assess accumulation of publications published over time:
year_totals <- year_totals %>%
  mutate(
    cumulative = cumsum(year_total),
    total = sum(year_total),
    cumulative_prop = cumulative / total
  )

# Find the year where cumulative proportion reaches or exceeds 50%
halfway_year <- year_totals %>%
  filter(cumulative_prop >= 0.5) %>%
  slice(1)

# View result
cat("Year when cumulative publications reached 50% of total:", 
    halfway_year$year.of.publication, "\n")

cat("Cumulative proportion by then:", round(halfway_year$cumulative_prop, 3), 
    "\n")

cat("Cumulative publications by then:", halfway_year$cumulative, "of", 
    halfway_year$total, "\n")


# Identify the average publications per year
pub_year_summary <- database %>%
  filter(study.type != "Meta-analysis") %>%
  group_by(year.of.publication, climate_focus) %>%
  summarise(n = n(), .groups = "drop") %>%
  complete(year.of.publication, climate_focus, fill = list(n = 0)) %>%
  group_by(year.of.publication) %>%
  mutate(
    year_total = sum(n),
    year_mean = mean(n),
    year_median = median(n)
  ) %>%
  ungroup()



#################################################
# Publication Count of trial time period        #
database <- database %>%
  mutate(
    trial_years = str_extract_all(trial.time.period, "\\d{4}"),
    trial_years = lapply(trial_years, as.numeric),
    latest_trial_year = sapply(trial_years, function(x) {
      if (length(x) > 0) max(x) else NA
    })
  )

# Count of studies by year: Research and Publication ##########
trial_year_summary <- database %>%
  filter(!is.na(latest_trial_year), study.type != "Meta-analysis") %>%
  count(latest_trial_year, name = "n")

# Convert publication year to numeric
pub_year_summary <- pub_year_summary %>%
  mutate(year.of.publication = as.numeric(as.character(year.of.publication)))




#####################################################
# Count: Discursive Context of Climate Change #
climate_framing <- database %>%
  mutate(
    mit = framing.climate.change.mit == "Y",
    adapt = framing.climate.change.adapt == "Y",
    overall = case_when(
      `framing.on.climate.change.mitigation/adaptation` == "N" ~ "No",
      mit & adapt ~ "Both",
      mit & !adapt ~ "Mitigation only",
      !mit & adapt ~ "Adaptation only",
      TRUE ~ "Unclear"
    )
  ) %>%
  count(overall, name = "n") %>%
  mutate(
    percent = 100 * n / sum(n),
    label = paste0(overall, "\n", round(percent, 1), "%")
  )


############################
#### Count of countries ####
country_counts <- database %>%
  filter(study.type != "Meta-analysis") %>%
  mutate(
    location.country = str_replace_all(location.country, "South Africa", "South_Africa"),
    location.country = str_replace_all(location.country, "United States", "United_States"),
    location.country = str_replace_all(location.country, "Spain \\(not stated\\)", "Spain"),
    location.country = str_replace_all(location.country, "Spain Palestinian territories", "Spain, Palestinian territories"),
    location.country = str_replace_all(location.country, "\\(not stated\\)", "(not stated)"),
    location.country = str_replace_all(location.country, "Palestinian territories", "Palestinian_territories"),
    location.country = str_replace_all(location.country, "Global Mediterranean", "Global_Mediterranean"),
    location.country = str_replace_all(location.country, "\\b([A-Z][a-z]+) ([A-Z][a-z]+)\\b", "\\1, \\2"),
  ) %>%
  select(location.country, climate_focus, article.ID) %>%
  filter(!is.na(location.country), !is.na(climate_focus)) %>%
  separate_rows(location.country, sep = ",|;| and | & |\\s(?=[A-Z])") %>%
  mutate(location.country = str_trim(location.country)) %>%
  filter(location.country != "") %>%
  distinct(article.ID, climate_focus, location.country) %>%
  count(location.country, climate_focus, sort = TRUE) %>%
  mutate(location.country = str_replace_all(location.country, "_", " "))

country_counts <- country_counts %>%
  group_by(location.country) %>%
  mutate(total_studies = sum(n)) %>%
  ungroup()

print(country_counts)

country_counts_wide <- country_counts %>%
  select(-total_studies) %>% # remove total_studies if present, we'll recompute
  pivot_wider(
    names_from = climate_focus,
    values_from = n,
    values_fill = 0
  ) %>%
  mutate(
    total_studies = Adaptation + Mitigation + Both
  ) %>%
  select(location.country, total_studies, Adaptation, Mitigation, Both)

print(country_counts_wide)

# Assess how much mitigation is more than adaptation##
more_mitigation <- country_counts_wide %>%
  filter(Mitigation > Adaptation) %>%
  summarise(n_countries = n())

print(more_mitigation)

# ==== 03 visualisations.publication_country.counts.R ====
# Visuals: Counts

#######################################################
#   Visualisation: Publication and research over time #

# Ensure all years present even if missing in data
all_years <- seq(
  min(c(pub_year_summary$year.of.publication, trial_year_summary$latest_trial_year), 
      na.rm = TRUE),
  max(c(pub_year_summary$year.of.publication, trial_year_summary$latest_trial_year), 
      na.rm = TRUE)
)

# Add missing years to pub_year_summary
pub_year_summary <- merge(
  expand.grid(year.of.publication = all_years, climate_focus = 
                c("Adaptation", "Mitigation", "Both")),
  pub_year_summary,
  by = c("year.of.publication", "climate_focus"),
  all.x = TRUE
)

pub_year_summary$n[is.na(pub_year_summary$n)] <- 0
pub_year_summary$climate_focus <- factor(
  pub_year_summary$climate_focus,
  levels = c("Both", "Mitigation", "Adaptation")
)

# Add missing years to trial_year_summary
trial_year_summary <- merge(
  data.frame(latest_trial_year = all_years),
  trial_year_summary,
  by = "latest_trial_year",
  all.x = TRUE
)
trial_year_summary$n[is.na(trial_year_summary$n)] <- 0

# All years to ensure x-axis is complete
all_years <- seq(
  min(c(pub_year_summary$year.of.publication, trial_year_summary$latest_trial_year), 
      na.rm = TRUE),
  max(c(pub_year_summary$year.of.publication, trial_year_summary$latest_trial_year), 
      na.rm = TRUE)
)

# Plot
ggplot() +
  # Bar chart for published focus
  geom_col(
    data = pub_year_summary,
    aes(x = year.of.publication, y = n, fill = climate_focus),
    position = "stack",
    width = 0.6
  ) +
  # Line + points for last year of trial
  geom_line(
    data = trial_year_summary,
    aes(x = latest_trial_year, y = n, color = "Last Year of Data Collection"),
    size = 0.8
  ) +
  geom_point(
    data = trial_year_summary,
    aes(x = latest_trial_year, y = n, color = "Last Year of Data Collection"),
    shape = 1,
    size = 2
  ) +
  # Manual fill and color
  scale_fill_manual(
    values = c("Adaptation" = "#B891B3", "Mitigation" = "#998888", "Both" = "#66A397"),
    labels = c(
      "Adaptation" = "Adaptation (published)",
      "Mitigation" = "Mitigation (published)",
      "Both" = "Both Mitigation and Adaptation (published)"
    )
  ) +
  scale_color_manual(
    values = c("Last Year of Data Collection" = "black"),
    guide = guide_legend(override.aes = list(linetype = 1, shape = 1))
  ) +
  # Axes formatting
  scale_y_continuous(name = "Number of Articles", breaks = seq(0, 
    max(c(pub_year_summary$n, trial_year_summary$n)), 5)) +
  scale_x_continuous(breaks = all_years, name = NULL) +
  # Theme styling
  theme_minimal(base_size = 16) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.title = element_blank(),
    legend.position = c(0.05, 0.95),
    legend.justification = c("left", "top"),
    legend.background = element_blank(),
    plot.title = element_blank(),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA)
  )

ggsave(
  filename = "climate_focus_plot.png", # or use .pdf or .tiff
  plot = last_plot(), # saves the last ggplot object
  width = 12, # in inches
  height = 8,
  dpi = 600, # high resolution
  units = "in"
)

########################
# Plot M/A/Both of studies as donut
ggplot(Climtype_counts, aes(x = 2, y = n, fill = climate_focus)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  xlim(0.5, 2.5) +
  theme_void(base_size = 16) +
  theme(legend.position = "none") +
  geom_text(aes(label = label),
    position = position_stack(vjust = 0.5),
    size = 5, color = "white"
  ) +
  scale_fill_manual(values = plot_colors) +
  labs()

ggsave(
  filename = "climate_type_plot.png", # or use .pdf or .tiff
  plot = last_plot(), # saves the last ggplot object
  width = 12, # in inches
  height = 8,
  dpi = 600, # high resolution
  units = "in"
)


# Define custom colors (you can adjust these!)
colors <- c(
  "Mitigation only" = "#998888",
  "Adaptation only" = "#B891B3",
  "Both" = "#66A397",
  "No" = "gray70",
  "Unclear" = "gray40"
)

# Donut plot
ggplot(climate_framing, aes(x = 2, y = n, fill = overall)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  xlim(0.5, 4) +
  theme_void(base_size = 16) +
  geom_text(aes(label = label),
    position = position_stack(vjust = 0.5),
    size = 5, color = "black"
  ) +
  scale_fill_manual(values = colors) +
  theme(legend.position = "none")
labs()

ggsave(
  filename = "climate_framing_plot.png", # or use .pdf or .tiff
  plot = last_plot(), # saves the last ggplot object
  width = 12, # in inches
  height = 8,
  dpi = 600, # high resolution
  units = "in"
)

# Plot Country Counts per Mitigation/Adaptation/Both
ggplot(country_counts, aes(x = reorder(location.country, -n, sum), y = n, 
                           fill = climate_focus)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = n),
    position = position_stack(vjust = 0.5), # centers labels vertically in each stack
    color = "black",
    size = 4
  ) + # adjust size as needed
  scale_fill_manual(values = c("Adaptation" = "#B891B3", "Mitigation" = "#998888", "Both" = "#66A397")) +
  labs(x = "", y = "Number of Studies", fill = "") +
  scale_y_continuous(breaks = seq(0, max(country_counts$n), by = 5)) +
  theme_minimal(base_size = 16) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
ggsave(
  filename = "country_climatetype_plot.png", # or use .pdf or .tiff
  plot = last_plot(), # saves the last ggplot object
  width = 12, # in inches
  height = 8,
  dpi = 600, # high resolution
  units = "in"
)

# ==== 04 crop.count.R ====
###########################
###  Primary Crops studied      ##
target_crops <- c(
  "Wheat", "Grape", "Tomato", "Olive", "Barley", "Maize", "Almond", "Lettuce",
  "Chickpea", "Faba bean", "Cauliflower", "Fennel", "Proso millet", "Canola",
  "Alfalfa", "Lupin", "Cowpea", "Cullen", "Soybean", "Mugwort",
  "Apricot", "Lentil", "Blueberry", "Camelina", "Carob", "Zucchini", "Peppers",
  "Clover", "Savoy cabbage", "Glass wort", "Grapefruit", "Sorghum",
  "Moringa", "Orange", "Peach", "French serradella", "Pistachio", "Potato",
  "Quinoa", "Rooibos", "Safflower", "Saffron", "St Johns wort",
  "Sulla", "Teff", "Oat", "Ryegrass", "Cocksfoot", "Tall fescue", "Artemisia",
  "Mandarin", "Triticale", "Cardoon", "Sunflower", "Rapeseed", "Carrot"
)
target_crops_clean <- tools::toTitleCase(tolower(target_crops))
# mandarin triticale cardoon sunflower rapeseed carrot

crop_type <- tribble(
  ~crop_Type, ~crop,
  "Beverage crop", "Rooibos",
  "Cereal", "Barley",
  "Cereal", "Proso Millet",
  "Cereal", "Sorghum",
  "Cereal", "Quinoa",
  "Cereal", "Teff",
  "Cereal", "Oat",
  "Cereal", "Triticale",
  "Cereal", "Wheat",
  "Cereal", "Maize",
  "Fruit", "Grape",
  "Fruit", "Olive",
  "Fruit", "Mandarin",
  "Fruit", "Apricot",
  "Fruit", "Blueberry",
  "Fruit", "Orange",
  "Fruit", "Peach",
  "Fruit", "Grapefruit",
  "Grass", "Ryegrass",
  "Grass", "Cocksfoot",
  "Grass", "Tall Fescue",
  "Herb", "Artemisia",
  "Herb", "Mugwort",
  "Herb", "St Johns Wort",
  "Herb", "Sulla",
  "Legume", "Chickpea",
  "Legume", "Faba Bean",
  "Legume", "Alfalfa",
  "Legume", "Lupin",
  "Legume", "Cowpea",
  "Legume", "Cullen",
  "Legume", "Soybean",
  "Legume", "Lentil",
  "Legume", "Carob",
  "Legume", "Moringa",
  "Legume", "French Serradella",
  "Legume", "Clover",
  "Nuts", "Almond",
  "Nuts", "Pistachio",
  "Oil", "Camelina",
  "Oil", "Safflower",
  "Oil", "Canola",
  "Oil", "Rapeseed",
  "Oil", "Sunflower",
  "Root crop", "Potato",
  "Spice", "Saffron",
  "Spice", "Fennel",
  "Vegetable", "Cauliflower",
  "Vegetable", "Zucchini",
  "Vegetable", "Peppers",
  "Vegetable", "Savoy Cabbage",
  "Vegetable", "Glass Wort",
  "Vegetable", "Tomato",
  "Vegetable", "Lettuce",
  "Vegetable", "Carrot",
  "Vegetable", "Cardoon",
)

latin_crop <- tribble(
  ~crop, ~latin_name,
  "Barley", "Hordeum vulgare",
  "Proso millet", "Panicum miliaceum",
  "Sorghum", "Sorghum bicolor",
  "Quinoa", "Chenopodium quinoa",
  "Teff", "Eragrostis tef",
  "Oat", "Avena sativa",
  "Wheat", "Triticum spp",
  "Maize", "Zea mays",
  "Grape", "Vitis vinifera",
  "Apricot", "Prunus armeniaca",
  "Blueberry", "Vaccinium corymbosum",
  "Peach", "Prunus persica",
  "Olive", "Olea europaea",
  "Orange", "Citrus x sinensis",
  "Grapefruit", "Citrus x paradisi",
  "Ryegrass", "Lolium spp",
  "Cocksfoot", "Dactylis glomerata",
  "Tall fescue", "Festuca arundinacea",
  "Rooibos", "Aspalathus linearis",
  "Artemisia", "Artemisia absinthium",
  "Mugwort", "Artemisia vulgaris",
  "St Johns wort", "Hypericum perforatum",
  "Sulla", "Sulla coronaria",
  "Chickpea", "Cicer arietinum",
  "Faba bean", "Vicia faba",
  "Alfalfa", "Medicago sativa",
  "Lupin", "Lupinus spp",
  "Cowpea", "Vigna unguiculata",
  "Cullen", "Cullen spp",
  "Soybean", "Glycine max",
  "Lentil", "Lens culinaris",
  "Carob", "Ceratonia siliqua",
  "Moringa", "Moringa oleifera",
  "French serradella", "Ornithopus sativus",
  "Clover", "Trifolium spp",
  "Almond", "Prunus dulcis",
  "Pistachio", "Pistacia vera",
  "Camelina", "Camelina sativa",
  "Safflower", "Carthamus tinctorius",
  "Canola", "Brassica napus",
  "Potato", "Solanum tuberosum",
  "Saffron", "Crocus sativus",
  "Fennel", "Foeniculum vulgare",
  "Cauliflower", "Brassica oleracea",
  "Zucchini", "Cucurbita pepo",
  "Peppers", "Capsicum spp",
  "Savoy cabbage", "Brassica oleracea var. sabauda",
  "Glass wort", "Salicornia spp",
  "Tomato", "Solanum lycopersicum",
  "Lettuce", "Lactuca sativa",
  "Mandarin", "Citrus reticulata",
  "Cardoon", "Cynara cardunculus",
  "Carrot", "Daucus carota",
  "Sunflower", "Helianthus annuus",
  "Rapeseed", "Brassica napus",
  "Triticale", "× Triticosecale"
)


# Step 1: Join crop_type and latin_crop to get a complete crop table
crop_data <- crop_type %>%
  full_join(latin_crop, by = "crop")

# Step 2: Filter to only keep crops in your target_crops list
crop_table <- crop_data %>%
  filter(crop %in% target_crops_clean) %>%
  arrange(crop_Type, crop)

# View the result
print(crop_table)

n <- nrow(crop_table)
half <- ceiling(n / 2)

left <- crop_table %>%
  slice(1:half) %>%
  rename(
    `Crop Type` = crop_Type,
    `Crop` = crop,
    ` ` = latin_name
  )

right <- crop_table %>%
  slice((half + 1):n) %>%
  rename(
    `Crop Type` = crop_Type,
    `Crop` = crop,
    `  ` = latin_name # two spaces so headers are distinct blanks
  )

if (nrow(right) < nrow(left)) {
  n_pad <- nrow(left) - nrow(right)
  # Create a tibble of NAs with same columns as right
  pad <- right[rep(1, n_pad), ]
  pad[, ] <- NA
  right <- bind_rows(right, pad)
}

double_col_table <- bind_cols(left, right)


ft <- double_col_table %>%
  flextable() %>%
  italic(j = c(3, 6)) %>%
  fontsize(size = 8, part = "all") %>%
  padding(padding = 1, part = "all") %>%
  autofit()

# Export to Word in landscape mode
read_docx() %>%
  body_add_flextable(value = ft) %>%
  print(target = "crop_table.docx")

#### Crop Count#########
crop_counts <- database %>%
  filter(!is.na(primary.crop.of.focus)) %>%
  mutate(
    matched_crops = map(primary.crop.of.focus, function(text) {
      text_lower <- tolower(text)
      found <- target_crops_clean[
        str_detect(text_lower, regex(tolower(target_crops_clean), ignore_case = TRUE))
      ]
      return(found)
    })
  ) %>%
  unnest(matched_crops) %>%
  count(matched_crops, name = "count") %>%
  arrange(desc(count)) %>%
  mutate(crop = str_to_title(matched_crops)) %>%
  select(crop, count)


# Crop count per country:

crop_counts_by_country <- database %>%
  filter(!is.na(primary.crop.of.focus), !is.na(location.country)) %>%
  # Step 1: Standardize country names to prevent incorrect splitting
  mutate(
    location.country = str_replace_all(location.country, "South Africa", "South_Africa"),
    location.country = str_replace_all(location.country, "United States", "United_States"),
    location.country = str_replace_all(location.country, "Spain \\(not stated\\)", "Spain"),
    location.country = str_replace_all(location.country, "Spain Palestinian territories", "Spain, Palestinian territories"),
    location.country = str_replace_all(location.country, "\\(not stated\\)", "(not stated)"),
    location.country = str_replace_all(location.country, "Palestinian territories", "Palestinian_territories"),
    location.country = str_replace_all(location.country, "Global Mediterranean", "Global_Mediterranean"),
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
  # Step 2: Split combined country names
  separate_rows(location.country, sep = ",|;| and | & |\\s(?=[A-Z])") %>%
  # Step 3: Clean and restore country names
  mutate(
    location.country = str_trim(location.country),
    location.country = str_replace_all(location.country, "_", " "),
    crop = str_to_title(matched_crops)
  ) %>%
  # Step 4: Remove empty or invalid entries
  filter(location.country != "") %>%
  # Step 5: Ensure unique combinations to avoid double-counting
  distinct(article.ID, crop, location.country, .keep_all = TRUE) %>%
  # Step 6: Count crops by country
  count(location.country, crop, name = "count") %>%
  # Step 7: Add total studies per country
  group_by(location.country) %>%
  mutate(total_studies = sum(count)) %>%
  ungroup() %>%
  arrange(location.country, desc(count)) %>%
  select(location.country, crop, count, total_studies)

# ==== 05 visualisation.crop.count.R ====
# visual crop count of crops frequency of >1##


crop_counts_filtered <- crop_counts %>%
  filter(!str_detect(crop, regex("^cereal\\b", ignore_case = TRUE))) %>%
  filter(count > 1)

ggplot(crop_counts_filtered, aes(x = count, y = reorder(crop, count))) +
  geom_col(fill = "black", width = 0.7) +
  geom_text(
    aes(label = count),
    position = position_nudge(x = 1),
    hjust = 0,
    vjust = 0.5,
    size = 5
  ) +
  scale_x_continuous(
    breaks = seq(0, max(crop_counts_filtered$count) + 5, by = 5),
    expand = expansion(mult = c(0, 0.05))
  ) +
  theme_minimal(base_size = 16) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA),
    axis.ticks = element_line(color = "black"),
    plot.margin = margin(1, 1, 1, 1.5, "cm")
  ) +
  labs(x = "Number of Studies", y = NULL)

ggsave(
  filename = "crop_count_plot.png", # or use .pdf or .tiff
  plot = last_plot(), # saves the last ggplot object
  width = 12, # in inches
  height = 8,
  dpi = 600, # high resolution
  units = "in"
)

# ==== 06 mitigation_adaptation.outcomes.R ====
#########################################
### Climate change mitigation outcome  ##
climtype_counts <- database %>%
  filter(study.type != "Meta-analysis") %>%
  count(climate_focus) %>%
  mutate(percent = round(n / sum(n) * 100, 1))

print(climtype_counts)

#########################################
### Climate change mitigation outcome ##
expected_combos <- c(
  "SOC", "N2O", "CH4", "CO2",
  "SOC N2O", "SOC CH4", "SOC CO2",
  "N2O CH4", "N2O CO2", "CH4 CO2",
  "SOC N2O CH4", "SOC N2O CO2", "SOC CH4 CO2",
  "N2O CH4 CO2", "SOC N2O CH4 CO2", "None"
)

# Main pipeline
mit_counts <- database %>%
  filter(!is.na(mitigation.aspect.studied)) %>%
  mutate(
    SOC = if_else(str_detect(mitigation.aspect.studied, regex("SOC", ignore_case = TRUE)), "SOC", NA_character_),
    N2O = if_else(str_detect(mitigation.aspect.studied, regex("N2O", ignore_case = TRUE)), "N2O", NA_character_),
    CH4 = if_else(str_detect(mitigation.aspect.studied, regex("CH4", ignore_case = TRUE)), "CH4", NA_character_),
    CO2 = if_else(str_detect(mitigation.aspect.studied, regex("CO2", ignore_case = TRUE)), "CO2", NA_character_),
    mit_Combo = paste(
      ifelse(!is.na(SOC), "SOC", ""),
      ifelse(!is.na(N2O), "N2O", ""),
      ifelse(!is.na(CH4), "CH4", ""),
      ifelse(!is.na(CO2), "CO2", "")
    ),
    mit_Combo = str_squish(mit_Combo),
    mit_Combo = if_else(mit_Combo == "", "None", mit_Combo)
  ) %>%
  count(mit_Combo, sort = FALSE) %>%
  complete(mit_Combo = expected_combos, fill = list(n = 0)) %>%
  arrange(desc(n)) %>%
  filter(mit_Combo != "None") ### None come from the adaptation only related studies.


print(mit_counts)




#########################################
### Climate change adaptation outcome ##
adaptation_labels <- c(
  "water stress",
  "temperature stress",
  "salinity",
  "acidic soil",
  "nutrient management",
  "soil health (biodiversity)",
  "soil health (physical)",
  "yield",
  "quality",
  "increased efficiency",
  "weeds",
  "pathogens",
  "pests",
  "heavy metals",
  "shade",
  "traits"
)

# Corrected counting approach
adaptation_counts <- map_dfr(adaptation_labels, ~ {
  database %>%
    filter(!is.na(primary.adaptation.aspect.studied)) %>%
    summarize(
      term = .x,
      count = sum(str_detect(
        primary.adaptation.aspect.studied,
        fixed(.x, ignore_case = TRUE)
      ))
    )
}) %>%
  arrange(desc(count))

# View results
print(adaptation_counts)

# prep for plotting #
adaptation_counts <- adaptation_counts %>%
  # 1) reorder *ascending* so the largest count is the highest y-level (i.e. top)
  mutate(term = fct_reorder(term, count))

# ==== 07 visualisation.mitigation_adaptation.outcomes.R ====
#######################################
# Mitigation outcome #

# Assign group_label in plain text.
# note "Other/Unclear" are N/A (adaptation linked)
mit_counts_grouped <- mit_counts %>%
  mutate(
    has_SOC = str_detect(mit_Combo, "SOC"),
    has_N2O = str_detect(mit_Combo, "N2O"),
    has_CH4 = str_detect(mit_Combo, "CH4"),
    has_CO2 = str_detect(mit_Combo, "CO2"),
    count = has_SOC + has_N2O + has_CH4 + has_CO2,
    group_label = case_when(
      count == 1 & has_SOC ~ "Soil organic C only",
      count == 1 & has_N2O ~ "N???O only",
      count == 1 & has_CH4 ~ "CH??? only",
      count == 1 & has_CO2 ~ "CO??? only",
      count == 4 ~ "All gases and Soil organic C",
      count == 2 ~ "Combination of any two",
      count == 3 ~ "Combination of any three",
      TRUE ~ "Other"
    )
  ) %>%
  filter(group_label != "Other") %>%
  group_by(group_label) %>%
  summarise(n = sum(n), .groups = "drop")


# Plot

labels_parsed <- c(
  "Soil organic C only" = "Soil~organic~C~only",
  "N???O only" = "N[2]*O~only",
  "CH??? only" = "CH[4]~only",
  "CO??? only" = "CO[2]~only",
  "Combination of any two" = "Combination~of~any~two",
  "Combination of any three" = "Combination~of~any~three",
  "All gases and Soil organic C" = "All~gases~and~Soil~organic~C"
)

# Ensure the order of bars matches your table, descending by count
mit_counts_grouped %>%
  mutate(group_label = factor(group_label, levels = names(labels_parsed))) %>%
  ggplot(aes(x = group_label, y = n)) +
  geom_bar(stat = "identity", fill = "#66A397") +
  geom_text(aes(label = n), vjust = -0.3, color = "black", size = 4.5) +
  labs(
    x = "Mitigation Aspect Group",
    y = "Number of Studies"
  ) +
  scale_x_discrete(labels = function(x) parse(text = labels_parsed[x])) +
  theme_minimal(base_size = 16) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA)
  )

ggsave(
  filename = "mitigation_outcome_plot.png", # or use .pdf or .tiff
  plot = last_plot(), # saves the last ggplot object
  width = 12, # in inches
  height = 8,
  dpi = 600, # high resolution
  units = "in"
)

####################################################
# 2. Adaptation Outcomes    ##

label_conversion <- c(
  "water stress" = "Resilience to water stress",
  "temperature stress" = "Resilience to temperature stress",
  "salinity" = "Resilience to salinity",
  "acidic soil" = "Resilience to acidic soil",
  "nutrient management" = "Enhanced nutrient management",
  "soil health (biodiversity)" = "Enhanced soil health (biodiversity)",
  "soil health (physical)" = "Enhanced soil health (physical)",
  "yield" = "Improved yield",
  "quality" = "Improved quality",
  "increased efficiency" = "Increased efficiency",
  "weeds" = "Resilience against weeds",
  "pathogens" = "Resilience against pathogens",
  "pests" = "Resilience against pests",
  "heavy metals" = "Resilience against heavy metals",
  "shade" = "Resilience to microclimates: shade",
  "trait" = "Improved traits"
)
# Now apply to the plot
adaptation_counts <- adaptation_counts %>%
  mutate(term = recode(term, !!!label_conversion))

ggplot(adaptation_counts, aes(x = count, y = term)) +
  geom_col(fill = "#B891B3", width = 0.7) +
  geom_text(
    aes(label = count),
    position = position_nudge(x = 1),
    hjust = 0,
    vjust = 0.5,
    size = 5
  ) +
  scale_x_continuous(
    breaks = seq(0, max(adaptation_counts$count) + 10, by = 10),
    expand = expansion(mult = c(0, 0.05))
  ) +
  scale_y_discrete() + # No need for labels argument now
  theme_minimal(base_size = 16) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA),
    axis.ticks = element_line(color = "black"),
    plot.margin = margin(1, 1, 1, 1.5, "cm")
  ) +
  labs(x = "Number of Studies", y = NULL)

# Save high-res
ggsave(
  filename = "adaptation_count_plot.png",
  plot = last_plot(),
  width = 12,
  height = 8,
  dpi = 600,
  units = "in"
)

# ==== 08 intervention.count.R ====
#####################################
######  Management interventions########
intervention_cols <- c(
  "agroforestry",
  "biochar.application",
  "biostimulant",
  "chemical.fertiliser.management",
  "cover.crops",
  "crop.rotation",
  "crop.variety.selection",
  "cultivar.selection/selective.breeding",
  "harvesting.frequency",
  "harvesting.time",
  "hedgerow.application",
  "hydraulic.arrangement",
  "inorganic.amendment",
  "intercropping",
  "irrigation.management",
  "livestock.integration",
  "nitrogen.inhibitor",
  "organic.fertiliser.management",
  "planting.density",
  "planting.technology",
  "soil.cover: residue management, mulch",
  "solarised.soil",
  "sowing.timing",
  "superabsorbent.polymer",
  "symbiotic.amendment.(fungi/bacteria)",
  "tillage.management",
  "weed.management"
)

## Groupings ###
intervention_groups <- c(
  "tillage.management" = "Crop management system",
  "cover.crops" = "Crop management system",
  "weed.management" = "Crop management system",
  "organic.fertiliser.management" = "Soil inputs",
  "chemical.fertiliser.management" = "Soil inputs",
  "biochar.application" = "Soil inputs",
  "biostimulant" = "Soil inputs",
  "inorganic.amendment" = "Soil inputs",
  "nitrogen.inhibitor" = "Soil inputs",
  "symbiotic.amendment.(fungi/bacteria)" = "Soil inputs",
  "soil.cover: residue management, mulch" = "Soil inputs",
  "superabsorbent.polymer" = "Soil inputs",
  "solarised.soil" = "Soil inputs",
  "irrigation.management" = "Water management",
  "hydraulic.arrangement" = "Water management",
  "crop.variety.selection" = "crop establishment and harvesting",
  "crop.rotation" = "Crop management system",
  "cultivar.selection/selective.breeding" = "crop establishment and harvesting",
  "sowing.timing" = "crop establishment and harvesting",
  "planting.density" = "crop establishment and harvesting",
  "planting.technology" = "crop establishment and harvesting",
  "harvesting.time" = "crop establishment and harvesting",
  "harvesting.frequency" = "crop establishment and harvesting",
  "intercropping" = "Crop management system",
  "agroforestry" = "Crop management system",
  "hedgerow.application" = "Crop management system",
  "livestock.integration" = "Crop management system"
)




# Convert from wide to long format
intervention_counts <- database %>%
  select(all_of(intervention_cols), climate_focus) %>%
  pivot_longer(
    cols = all_of(intervention_cols), names_to = "Intervention",
    values_to = "Studied"
  ) %>%
  filter(Studied == "Y") %>%
  # Add group information
  mutate(Group = intervention_groups[Intervention]) %>%
  group_by(Group, Intervention, climate_focus) %>%
  summarise(Count = n(), .groups = "drop") %>%
  pivot_wider(names_from = climate_focus, values_from = Count, values_fill = 0) %>%
  mutate(Total = rowSums(across(c("Adaptation", "Mitigation", "Both")))) %>%
  arrange(Group, desc(Total))

print(intervention_counts)

## Plot
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


intervention_counts <- database %>%
  select(all_of(intervention_cols), climate_focus) %>%
  pivot_longer(
    cols = all_of(intervention_cols),
    names_to = "Intervention",
    values_to = "Studied"
  ) %>%
  filter(Studied == "Y") %>%
  # Clean names and add groups
  mutate(
    Intervention = clean_intervention_names(Intervention),
    Group = intervention_groups[Intervention]
  ) %>%
  group_by(Group, Intervention, climate_focus) %>%
  summarise(Count = n(), .groups = "drop") %>%
  pivot_wider(
    names_from = climate_focus,
    values_from = Count,
    values_fill = 0
  ) %>%
  mutate(Total = rowSums(across(c("Adaptation", "Mitigation", "Both")))) %>%
  arrange(Group, desc(Total))

long_data_intervention <- intervention_counts %>%
  pivot_longer(
    cols = c(Adaptation, Mitigation, Both),
    names_to = "type",
    values_to = "Count"
  )

# percentage of the group calculation#
group_totals <- intervention_counts %>%
  group_by(Group) %>%
  summarise(GroupTotal = sum(Total), .groups = "drop")
overall_total <- sum(group_totals$GroupTotal)
group_totals <- group_totals %>%
  mutate(GroupLabel = paste0(
    Group, "
                             (", round(100 * GroupTotal / overall_total, 1),
    "%)"
  ))
long_data_intervention <- long_data_intervention %>%
  left_join(group_totals, by = "Group")

# Calculations for Interventions linked to policy:

# Share of interventions that are both adaptation/mitigation
interven_total <- nrow(intervention_counts)
interven_both <- sum(intervention_counts$Both > 0)
share_both <- interven_both / interven_total
print(share_both)

# frequency of studies on interventions of both
#   (compared to adaptation, mitigation)
totalinterven_counts <- colSums(intervention_counts[
  ,
  c("Adaptation", "Mitigation", "Both")
])
totalinterven_counts

shareinterven_both_freq <- totalinterven_counts["Both"] / sum(totalinterven_counts)
print(shareinterven_both_freq)

# Count of studies by type where climate_focus == "Both"
both_counts <- database %>%
  filter(climate_focus == "Both") %>%
  count(study.type)

# Total number of meta-analyses
meta_total <- database %>%
  filter(study.type == "Meta-analysis") %>%
  nrow()

# Display results
both_counts
meta_total

# ==== 09 visualisation.intervention.count.R ====
ggplot(long_data_intervention, aes(
  y = reorder(Intervention, Total),
  x = Count,
  fill = type
)) +
  geom_bar(stat = "identity") +
  geom_text(
    aes(label = ifelse(Count > 0, Count, "")),
    position = position_stack(vjust = 0.5),
    color = "black",
    size = 4
  ) +
  facet_grid(GroupLabel ~ ., scales = "free_y", space = "free_y") +
  scale_fill_manual(values = c(
    "Adaptation" = "#B891B3",
    "Mitigation" = "#998888",
    "Both" = "#66A397"
  )) +
  labs(
    x = "Frequency",
    y = "",
    fill = ""
  ) +
  scale_x_continuous(expand = expansion(mult = c(0, 0.05))) +
  theme_minimal(base_size = 16) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 0.5),
    axis.text.y = element_text(size = 14, hjust = 1),
    strip.placement = "outside",
    strip.text = element_text(face = "bold", size = 12),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.justification = "right",
    legend.box = "horizontal",
    plot.margin = margin(t = 10, r = 10, b = 30, l = 10),
    strip.clip = "off",
    strip.background = element_blank()
  )

ggsave(
  filename = "intervention_climatetype_plot.png",
  plot = last_plot(),
  width = 14,
  height = 10,
  dpi = 600,
  units = "in"
)

# ==== 10 World Map final.R ====
install.packages(c("ggplot2", "sf", "rnaturalearth", "rnaturalearthdata", "ggrepel", "terra"))
library(sf)
library(ggplot2)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggrepel)
library(terra)

## latest, Grok 3 aug 1715
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
      climate == "8" ~ "#C8C800", # Csa - Yellow
      climate == "9" ~ "#C8C800", # Csb - Olive
      climate == "10" ~ "#C8C800" # Csc - Dark Olive
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
  filter(!is.na(total_studies)) %>% # Only keep matched countries
  distinct(name, .keep_all = TRUE) # Remove duplicates if any

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
  scale_fill_identity(
    guide = "legend", name = "Mediterranean Climate",
    labels = c("Csa", "Csb", "Csc"),
    breaks = c("#FFFF99", "#B3B300", "#666633")
  ) +
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

# ==== 11 Final working Sankey.R ====
# Sankey with only crop type, intervention and mit/adapt/both
  #Set up to ensure study is counted once...
install.packages("networkD3")
library(networkD3)

install.packages("webshot")
library(webshot)
webshot::install_phantomjs()

# country data set: country, crop, intervention, outcome, mitigation/adaptation
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
  distinct(article.ID, location.country, crop) %>%
  left_join(crop_data %>% select(crop, crop_Type), by = "crop")


# Step 3: Flatten intervention data per article
long_intervention_df <- database %>%
  select(article.ID, all_of(intervention_cols), climate_focus) %>%
  pivot_longer(cols = all_of(intervention_cols), names_to = "intervention", 
      values_to = "Studied") %>%
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
  inner_join(long_intervention_df, by = "article.ID", 
  relationship = "many-to-many") %>%
  mutate(
    climate_focus = type
  ) %>%
  select(article.ID, country = location.country, crop, crop_Type, 
  intervention, climate_focus)

head(sankey_data)

# Aggregate sankey_data at article.ID level to avoid double counting
sankey_data_aggregated <- sankey_data %>%
  group_by(article.ID, country, crop_Type, climate_focus) %>%
  summarise(
    interventions = paste(unique(intervention), collapse = ";"),
    .groups = "drop"
  ) %>%
  separate_rows(interventions, sep = ";") %>% # explode interventions into rows
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
  vwidth = 1200, # Width in pixels (adjust as needed)
  vheight = 800, # Height in pixels
  zoom = 2 # Doubles resolution (set to 3-4 for ultra-high quality)
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
  separate_rows(interventions, sep = ";") %>% # explode interventions into rows
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
# Ensure unique studies per climate focus
climate_focus_counts <- sankey_data_aggregated %>%
  distinct(article.ID, climate_focus) %>% 
  count(climate_focus) %>%
  filter(climate_focus %in% c("Adaptation", "Both", "Mitigation"))
print("Climate Focus Counts (Unique Studies):")
print(climate_focus_counts)

