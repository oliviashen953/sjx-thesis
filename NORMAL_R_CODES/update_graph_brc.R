library(readr)
library(dplyr)
library(ggplot2)
library(forcats)  # For reordering factors


# OLD FILES REFERENCE:
# ~/new_js_thesis/R4_OLDER_R_FILES/R4_STEP_GRAPH_BMI.R
library(readr)
best_pgs_columns_brc <- read_csv("NORMAL/brc_best_pgs/best_pgs_columns_brc.csv")
View(best_pgs_columns_brc)


# Count the number of occurrences for each value in the 'Best_PGS_Column'
occurrences <- table(best_pgs_columns_brc$Best_PGS_Column)

# Print the occurrences
print(occurrences)

# > print(occurrences)
# 
# PGS000007 PGS000008 PGS000212 PGS000214 PGS000347 PGS000527 PGS000529 PGS000532 PGS000536 
# 1         7         4         1         1         9         2         2         3 
# 

# Load the data
R4_brc_summary_odds_ratio_and_rr_across_groups <- read_csv("NORMAL_brc_summary_odds_ratio_and_rr_across_groups_and_iterations.csv")
brc_summary <- R4_brc_summary_odds_ratio_and_rr_across_groups
brc_summary


# Define the specific methods you're interested in
ra_methods <- c("mc4_rank", "mct_rank", "bcs_rank", "logs_rank", "sbs_rank", "avg_ra_rank")
pgs_methods <- c("lasso_rank", "ols_rank", "PGS000527_rank", "avg_pgs_rank")
all_methods <- c(ra_methods, pgs_methods)

# Filter to include only the specified methods
filtered_methods <- brc_summary %>%
  filter(rank_method %in% all_methods)

# Assign descriptive labels to rank_method
pgs_labels <- setNames(c(
  "RA method: MC4",
  "RA method: MCT",
  "RA method: Nuclear Norm bcs_rank",
  "RA method: Nuclear Norm logs_rank",
  "RA method: Nuclear Norm sbs_rank",
  "RA Average Rank",
  "Supervised3000 LASSO Rank",
  "Supervised3000 OLS Rank",
  "Supervised3000 Best PGS Model: PGS000527",
  "Average PGS Rank"
), all_methods)

# Apply the labels and convert rank_method to a factor
filtered_methods$rank_method <- factor(filtered_methods$rank_method, levels = all_methods, labels = pgs_labels)

# Reorder and rename the percentile factor
filtered_methods <- filtered_methods %>%
  mutate(percentile = fct_recode(percentile,
                                 "Top 5%" = "5%",
                                 "Top 10%" = "10%",
                                 "Top 20%" = "20%")) %>%
  mutate(percentile = fct_relevel(percentile, "Top 5%", "Top 10%", "Top 20%"))

# Generate a consistent color palette for methods across graphs
method_colors <- c(
  "RA method: MC4" = "#E41A1C", # Red
  "RA method: MCT" = "#377EB8", # Blue
  "RA method: Nuclear Norm bcs_rank" = "#4DAF4A", # Green
  "RA method: Nuclear Norm logs_rank" = "#984EA3", # Purple
  "RA method: Nuclear Norm sbs_rank" = "#FF7F00", # Orange
  "RA Average Rank" = "#FFFF33", # Yellow
  "Supervised3000 LASSO Rank" = "#A65628", # Brown
  "Supervised3000 OLS Rank" = "#F781BF", # Pink
  "Supervised3000 Best PGS Model: PGS000527" = "#999999", # Grey
  "Average PGS Rank" = "#66C2A5" # Teal
)

# Define shapes for each method
method_shapes <- c(
  "RA method: MC4" = 19,  # Circle
  "RA method: MCT" = 17,  # Triangle
  "RA method: Nuclear Norm bcs_rank" = 15,  # Square
  "RA method: Nuclear Norm logs_rank" = 18,  # Diamond
  "RA method: Nuclear Norm sbs_rank" = 16,  # Inverted Triangle
  "RA Average Rank" = 13,  # Plus
  "Supervised3000 LASSO Rank" = 8,  # Star
  "Supervised3000 OLS Rank" = 3,  # Cross
  "Supervised3000 Best PGS Model: PGS000527" = 4,  # X
  "Average PGS Rank" = 7  # Octagon
)

# Create the plot
plot <- ggplot(filtered_methods, aes(x = percentile, y = mean_odds_ratio, group = rank_method,
                                     shape = rank_method, color = rank_method)) +
  geom_point(size = 3) +
  geom_line() +
  scale_color_manual(values = method_colors) +
  scale_shape_manual(values = method_shapes) +
  theme_bw() +
  labs(y = "Mean Odds Ratio", x = "Group Percentile",
       title = "BRC (Supervised Random 3000): Comparison of Rank Methods Across Top Percentiles") +
  guides(color = guide_legend(override.aes = list(shape = unname(method_shapes))))

# Save or display the plot
plot
ggsave("NORMAL_brc_OG_Top_Percentiles.png", plot = plot, width = 10, height = 8, dpi = 300)

  