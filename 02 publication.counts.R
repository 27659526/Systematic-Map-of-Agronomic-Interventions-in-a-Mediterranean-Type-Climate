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
cat("Year when cumulative publications reached 50% of total:", halfway_year$year.of.publication, "\n")

cat("Cumulative proportion by then:", round(halfway_year$cumulative_prop, 3), "\n")

cat("Cumulative publications by then:", halfway_year$cumulative, "of", halfway_year$total, "\n")


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
    latest_trial_year = sapply(trial_years, function(x) 
      if (length(x) > 0) max(x) else NA)
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
  select(-total_studies) %>%  # remove total_studies if present, we'll recompute
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

#Assess how much mitigation is more than adaptation##
more_mitigation <- country_counts_wide %>%
  filter(Mitigation > Adaptation) %>%
  summarise(n_countries = n())

print(more_mitigation)
