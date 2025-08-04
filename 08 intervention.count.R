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
