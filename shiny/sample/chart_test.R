# https://themockup.blog/posts/2020-11-29-bullet-chart-variants-in-r/

library(tidyverse)
library(ggplot2)
library(ggtext)

load("shiny/final_model.RData")
final_model <- stable_lm_1

input_data <- data.frame(abdomen = 70, weight_kg = 60)
predicted_body_fat <- predict(final_model, input_data)

pct_bf_boundaries = c(21, 33, 39, 100)

pct_bf_df = tibble(
  name = "Body Fat %",
  value = predicted_body_fat
)

pct_bf_plot <- pct_bf_df %>%
  ggplot() +
  geom_col(
    aes(x = value, y = pct_bf_boundaries[4]),
    fill = "#ff6961",
    alpha = 0.9
  ) +
  geom_col(
    aes(x = value, y = pct_bf_boundaries[3]),
    fill = "#ffb54c",
    alpha = 0.9
  ) +
  geom_col(
    aes(x = value, y = pct_bf_boundaries[2]),
    fill = "#f8d66d",
    alpha = 0.9
  ) +
  geom_col(
    aes(x = value, y = pct_bf_boundaries[1]),
    fill = "#7abd7e",
    alpha = 0.9
  ) +
  geom_errorbar(
    aes(x = value, ymin = predicted_body_fat, ymax = predicted_body_fat),
    color = "#000000",
    width = 1.25,
    size = 1
  ) +
  coord_flip() +
  theme_minimal() +
  labs(
    x = "",
    y = "",
    title = paste("Your Predicted Body Fat Percentage: ", round(predicted_body_fat, 2)),
    subtitle = "[category]"
  ) +
  theme(
    panel.grid = element_blank(),
    plot.title = element_markdown(),
    plot.subtitle = element_markdown(),
    axis.text = element_text(face = "bold")
  ) +
  scale_y_continuous(breaks = seq(0, 100, 10)) +
  scale_x_discrete(breaks = c(1)) +
  theme(aspect.ratio = 1/6)