library(rlang)
library(ggstatsplot)
library(ggplot2)
library(dplyr)

dir.create("3_data_analysis/zj_NC_revision/Figure_S1", recursive = TRUE)
setwd("3_data_analysis/zj_NC_revision/Figure_S1")

# Create the dataset
data_arg <- data.frame(
  group = rep(c("Control", "UC"), each = 10),
  value = c(0.78, 0.68, 0.95, 1.25, 1.43, 1.14,
            0.91, 0.81, 1.51, 0.65, 6.9, 4.1,
            2.4, 3.1, 2.8, 6.4, 5.6, 4.1,
            3.5, 2.1)
)

data_nos2 <- data.frame(
  group = rep(c("Control", "UC"), each = 10),
  value = c(1.42, 0.68, 1.28, 0.69, 0.75, 0.89,
            0.92, 1.14, 1.38, 0.56, 1.25, 1.68,
            1.95, 3.24, 4.5, 5.1, 2.6, 3.4,
            1.92, 2.18)
)

# Convert group to factor with specified levels
data_arg  <- data_arg  %>%
  mutate(group = factor(group, levels = c("Control", "UC")))

data_nos2 <- data_nos2 %>%
  mutate(group = factor(group, levels = c("Control", "UC")))

# Create a theme_base if it doesn't exist
# This mimics a basic theme similar to what might be in the r4projects package
theme_base <- theme_bw() +
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    axis.text = element_text(color = "black"),
    legend.background = element_rect(fill = "white", color = NA)
  )


# Create the plot using ggbetweenstats
plot_arg <- ggbetweenstats(
  data = data_arg ,
  x = group,
  y = value,
  type = "nonparametric",  # Using nonparametric statistical test
  pairwise.comparisons = FALSE,  # No pairwise comparisons
  pairwise.display = "all",  # Display all comparisons (not used if pairwise.comparisons = FALSE)
  point.args = list(
    alpha = 0.9,
    size = 3,
    position = position_jitter(width = 0.1)
  ),
  centrality.label.args = list(size = 4),
  centrality.type = "parametric"  # Show mean instead of median
) +
  theme_base +  # Apply the base theme
  labs(x = "", y = "Relative ARG1 mRNA level") +  # Set axis labels
  theme(legend.position = "") +  # Remove the legend
  scale_color_manual(values = group_color)  # Apply custom colors

# Display the plot
print(plot_arg )

ggsave("ARG1_mRNA_level_plot.pdf", plot_arg , width = 6, height = 5)



# Create the plot using ggbetweenstats
plot_nos2 <- ggbetweenstats(
  data = data_nos2 ,
  x = group,
  y = value,
  type = "nonparametric",  # Using nonparametric statistical test
  pairwise.comparisons = FALSE,  # No pairwise comparisons
  pairwise.display = "all",  # Display all comparisons (not used if pairwise.comparisons = FALSE)
  point.args = list(
    alpha = 0.9,
    size = 3,
    position = position_jitter(width = 0.1)
  ),
  centrality.label.args = list(size = 4),
  centrality.type = "parametric"  # Show mean instead of median
) +
  theme_base +  # Apply the base theme
  labs(x = "", y = "Relative ARG1 mRNA level") +  # Set axis labels
  theme(legend.position = "") +  # Remove the legend
  scale_color_manual(values = group_color)  # Apply custom colors

# Display the plot
print(plot_nos2)

ggsave("NOS2_mRNA_level_plot.pdf", plot_nos2, width = 6, height = 5)
