library(dplyr)
library(ggstatsplot)

##ass1
ass1 <-
  rbind(
    data.frame(
      group = "Control",
      value = c(0.932496003, 0.945467085, 0.85711974)
    ),
    data.frame(
      group = "Control+C-01",
      value = c(0.843728177, 0.767791352, 0.713177981)
    ),
    data.frame(
      group = "Model",
      value = c(1.137810984, 1.119667318, 1.049997849)
    ),
    data.frame(
      group = "Model+C-01",
      value = c(1.033551823, 1.03744504, 0.790372832)
    )
  ) %>%
  dplyr::mutate(group = factor(
    group,
    levels = c(
      "Control",
      "Control+C-01",
      "Model",
      "Model+C-01"
    )
  ))

plot <-
  ggbetweenstats(
    data  = ass1,
    x  = group,
    y  = value,
    type = "parametric",
    pairwise.comparisons = TRUE,  # Enable pairwise comparisons
    pairwise.display = "all",
    p.adjust.method = "none",
    point.args = list(
      alpha = 0.9,
      size = 3,
      color = "black",
      position = position_jitter(width = 0.1)
    ),
    centrality.plotting = FALSE,  # This removes the red dot
    centrality.label.args = list(size = 4),
    centrality.type = "nonparametric"
  ) +
  theme_bw() +
  labs(x = "", y = "ASS1", title = "ASS1") +
  theme(legend.position = "",
        axis.text.x = element_text(angle = 45, hjust = 1))

plot


ggsave(plot,
       filename = "figure7e.pdf",
       width = 5,
       height = 5)


library(dplyr)
library(ggplot2)


# Define the comparisons for the significance bars
comparisons <- list(
  c("Control", "Control+C-01"),
  c("Control", "Model"),
  c("Model", "Model+C-01")
)

# Create the plot using ggplot2
plot <- ggplot(ass1, aes(x = group, y = value)) +
  geom_boxplot(outlier.shape = NA) +
  geom_point(position = position_jitter(width = 0.1), size = 2) +
  ggsignif::geom_signif(
    comparisons = comparisons,
    map_signif_level = TRUE,
    textsize = 3,
    tip_length = 0.01
  ) +
  theme_bw() +
  labs(x = "", y = "Relative protein level", title = "ASS1") +
  theme(
    legend.position = "",
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.y = element_line(color = "lightgray", linetype = "solid"),
    panel.grid.major.x = element_blank(),  # Remove vertical grid lines
    panel.grid.minor = element_blank()     # Remove all minor grid lines
  ) +
  scale_y_continuous(limits = c(0.7, 1.3))

# Add sample size annotation to x-axis labels
plot <- plot + scale_x_discrete(
  labels = c(
    "Control\n(n = 3)",
    "Control+C-01\n(n = 3)",
    "Model\n(n = 3)",
    "Model+C-01\n(n = 3)"
  )
)

print(plot)


ggsave(plot,
       filename = "figure7e.pdf",
       width = 5,
       height = 5)







plot <-
  ggbetweenstats(
    data  = ass1,
    x  = group,
    y  = value,
    type = "parametric",
    pairwise.comparisons = TRUE,  # Enable pairwise comparisons
    pairwise.display = "all",
    p.adjust.method = "none",
    point.args = list(
      alpha = 0.9,
      size = 3,
      color = "black",
      position = position_jitter(width = 0.1)
    ),
    centrality.plotting = FALSE,  # This removes the red dot
    centrality.label.args = list(size = 4),
    centrality.type = "nonparametric"
  ) +
  theme_bw() +
  labs(x = "", y = "ASS1", title = "ASS1") +
  theme(legend.position = "",
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(limits = c(0, max(ass1$value) * 1.1))  # Start from 0
plot
