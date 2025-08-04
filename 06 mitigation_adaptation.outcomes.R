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
