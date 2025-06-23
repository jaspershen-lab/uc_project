library(r4projects)
library(ggstatsplot)
setwd(get_project_wd())
rm(list = ls())
source('1_code/100_tools.R')


# First, let's create the dataframe from your data
p_mtor_mtor <- data.frame(
  group = rep(c("Control", "Model", "Arg"), each = 3),
  value = c(0.627902, 0.664195, 0.629197,
            0.94838, 0.948402, 0.894815,
            1.199815, 1.460173, 1.044177)
)



# Set the base theme if needed (assuming it's defined elsewhere)
# If not defined, you can use a standard theme like theme_minimal()
p_mtor_mtor$group <- factor(p_mtor_mtor$group, levels = c("Control","Model","Arg"))


# Create the plot
plot <- ggbetweenstats(
  data = p_mtor_mtor,
  x = group,
  y = value,
  type = "parametric",  # Parametric statistics
  pairwise.comparisons = FALSE,
  pairwise.display = "all",
  p.adjust.method = "none",
  point.args = list(
    alpha = 0.9,
    size = 3,
    position = position_jitter(width = 0.1)
  ),
  centrality.plotting = FALSE,
  # centrality.label.args = list(size = 4),
  centrality.type = "nonparametric"  # Using median instead of mean
) +
  theme_base +
  labs(x = "", y = "p-mTOR/mTOR") +
  theme(legend.position = "") +
  scale_color_manual(values = disease_color7)

# Display the plot
plot
