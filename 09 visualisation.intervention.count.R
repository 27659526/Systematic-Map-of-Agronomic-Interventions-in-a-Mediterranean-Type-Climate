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
