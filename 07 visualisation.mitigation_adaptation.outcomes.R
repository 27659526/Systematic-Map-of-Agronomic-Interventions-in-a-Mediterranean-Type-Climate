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
