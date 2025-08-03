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
