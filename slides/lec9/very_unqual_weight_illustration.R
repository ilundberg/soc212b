
library(tidyverse)
library(scales)

set.seed(90095)
sim <- tibble(weight = rlnorm(1000,0,1))

sim |>
  ggplot(aes(x = weight)) +
  geom_histogram(bins = 30) +
  theme_minimal() +
  labs(
    x = "Inverse Probability of Treatment Weight",
    y = "Frequency",
    title = "Hypothetical example: Very unequal weight. Histogram."
  )

ggsave("figures/very_unequal_weight_histogram.pdf", height = 3, width = 5)

with_cdf <- sim |>
  arrange(-weight) |>
  mutate(
    cdf_of_weight = cumsum(weight) / sum(weight),
    cdf_of_observations = 1:n() / n()
  )
with_cdf |>
  ggplot(aes(x = cdf_of_observations, y = cdf_of_weight)) +
  geom_line() +
  theme_minimal() +
  labs(
    x = "Cumulative Proportion of Observations",
    y = "Cumulative Proportion of Weight",
    title = "Hypothetical example: Very unequal weight. CDF."
  ) +
  geom_point(
    data = with_cdf |> 
      filter(cdf_of_observations == .1)
  ) +
  geom_text(
    data = with_cdf |> 
      filter(cdf_of_observations == .1) |>
      mutate(
        label = paste(
          "Example:",
          label_percent(accuracy = 1)(cdf_of_weight),
          "of weight (y-axis) falls on the\nmost heavily-weighted",
          label_percent(accuracy = 1)(cdf_of_observations),
          "of observations (x-axis)"
        )
      ),
    aes(label = label),
    hjust = 0, vjust = 1, nudge_x = .05
  ) +
  geom_segment(
    data = with_cdf |> 
      filter(cdf_of_observations == .1),
    aes(x = cdf_of_observations, y = 0, yend = cdf_of_weight),
    linetype = "dashed"
  ) +
  geom_segment(
    data = with_cdf |> 
      filter(cdf_of_observations == .1),
    aes(x = cdf_of_observations, xend = 0, y = cdf_of_weight),
    linetype = "dashed"
  )
ggsave("figures/very_unequal_weight_cdf.pdf", height = 3, width = 5)
