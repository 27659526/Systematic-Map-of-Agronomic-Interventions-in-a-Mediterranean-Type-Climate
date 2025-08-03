# Visuals: Counts

#######################################################
#   Visualisation: Publication and research over time #

# Ensure all years present even if missing in data
all_years <- seq(
  min(c(pub_year_summary$year.of.publication, trial_year_summary$latest_trial_year), na.rm = TRUE),
  max(c(pub_year_summary$year.of.publication, trial_year_summary$latest_trial_year), na.rm = TRUE)
)

# Add missing years to pub_year_summary
pub_year_summary <- merge(
  expand.grid(year.of.publication = all_years, climate_focus = c("Adaptation", "Mitigation", "Both")),
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
  min(c(pub_year_summary$year.of.publication, trial_year_summary$latest_trial_year), na.rm = TRUE),
  max(c(pub_year_summary$year.of.publication, trial_year_summary$latest_trial_year), na.rm = TRUE)
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
  scale_y_continuous(name = "Number of Articles", breaks = seq(0, max(c(pub_year_summary$n, trial_year_summary$n)), 5)) +
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
ggplot(country_counts, aes(x = reorder(location.country, -n, sum), y = n, fill = climate_focus)) +
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
