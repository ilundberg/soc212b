
# STREAMLINED ILLUSTRATE DOUBLE ROBUST

library(tidyverse)
theme_set(theme_minimal())

# More extreme DGP: Linear probabilities
data <- tibble(x = rep(1:9,1:9), a = F) |>
  bind_rows(tibble(x = rep(1:9,9:1), a = T)) |>
  group_by(x) |>
  mutate(
    pi = mean(a)
  ) |>
  ungroup() |>
  mutate(
    y1 = 9 - 9 * ((x - 5) / 5) ^ 2,
    y0 = 5,
    # Shift x to fit story of this DGP
    x = (x - 1) / 2 + 1,
    y = case_when(
      !a ~ y0,
      a ~ y1
    )
  )

data |>
  ggplot(aes(x = x, y = y, color = a)) +
  geom_jitter(width = .1, height = .1, alpha = .5) +
  scale_x_continuous(
    breaks = 1:10,
    name = "Height of Waves (feet)"
  ) +
  labs(
    y = "Awesomeness of Child's Day\n(Scale 1-10)"
  ) +
  scale_color_discrete(
    name = "Treatment",
    labels = c("Did not surf","Surfed")
  )
ggsave("figures/dr_y.pdf", height = 3, width = 5)

data |>
  ggplot(aes(x = x, y = pi)) +
  geom_line() +
  geom_point() +
  ggtitle("Propensity Score") +
  labs(
    x = "Height of Waves (feet)",
    y = "Probability that Parent\nAllows Child to Surf"
  )
ggsave("figures/dr_pscore.pdf", height = 3, width = 5)

data |>
  ggplot(aes(x = x, fill = a)) +
  geom_histogram(alpha = .8) +
  facet_wrap(~a, ncol = 1) +
  ggtitle("Density Within Treatment Groups") +
  theme_bw()

# Goal: ATC

to_predict_even_space <- tibble(x = seq(1,5,1))
fit1 <- lm(y ~ x, data = data |> filter(a))

fitted <- to_predict_even_space |>
  mutate(y = predict(
    fit1,
    newdata = to_predict_even_space
  )) |>
  mutate(set = "Best-fit line\nin observed data")

# Best-fit line among untreated
data |>
  ggplot(aes(x = x, y = y, color = a)) +
  geom_jitter(width = .1, height = .1, alpha = .5) +
  scale_x_continuous(
    breaks = 1:10,
    name = "Height of Waves (feet)"
  ) +
  labs(
    y = "Awesomeness of Child's Day\n(Scale 1-10)"
  ) +
  scale_color_discrete(
    name = "Treatment",
    labels = c("Did not surf","Surfed")
  ) + 
  # Add the fitted line
  geom_line(
    data = fitted |> mutate(a = TRUE)
  )

ggsave("figures/dr_bestFitLine.pdf", height = 3, width = 5)

# Visualize predicted effects
fitted <- data |>
  mutate(fitted = predict(fit1, newdata = data)) |>
  filter(!a)

atc_naive <- fitted |> summarize(atc_naive = mean(fitted - y))

fitted |>
  ggplot(aes(x = x, y = y, color = a)) +
  geom_jitter(width = .1, height = .1, alpha = .5) + 
  # Add the fitted line
  geom_line(
    data = fitted |> mutate(a = TRUE),
    aes(y = fitted)
  ) +
  geom_segment(aes(yend = fitted), linetype = "dashed", color = "gray") +
  scale_x_continuous(
    breaks = 1:10,
    name = "Height of Waves (feet)"
  ) +
  labs(
    y = "Awesomeness of Child's Day\n(Scale 1-10)"
  ) +
  scale_color_discrete(
    name = "Treatment",
    labels = c("Did not surf","Surfed")
  ) +
  ggtitle(paste(
    "ATC: On average, awesomness would increase\nby",
    round(atc_naive,2),
    "if I had surfed on the days I wasn't allowed."
  ))

ggsave("figures/dr_predictedeffects.pdf", height = 3, width = 5)

# Weighted errors
observed_error <- data |>
  mutate(fitted = predict(fit1, newdata = data)) |>
  filter(a) |>
  mutate(weight = (1 - pi) / pi)

observed_error |>
  ggplot(aes(x = x, y = y)) +
  geom_jitter(width = .1, height = .1, alpha = .5) + 
  # Add the fitted line
  geom_line(
    data = fitted,
    aes(y = fitted)
  ) +
  geom_segment(aes(yend = fitted), linetype = "dashed", color = "gray") +
  scale_x_continuous(
    breaks = 1:10,
    name = "Height of Waves (feet)"
  ) +
  labs(
    y = "Awesomeness of Child's Day\n(Scale 1-10)"
  ) +
  scale_color_discrete(
    name = "Treatment",
    labels = c("Did not surf","Surfed")
  )
ggsave("figures/dr_observedError.pdf", height = 3, width = 5)

observed_error |>
  ggplot(aes(x = x, y = y)) +
  geom_jitter(aes(size = weight), width = .1, height = .1, alpha = .5) + 
  # Add the fitted line
  geom_line(
    data = fitted,
    aes(y = fitted)
  ) +
  geom_segment(aes(yend = fitted), linetype = "dashed", color = "gray") +
  scale_x_continuous(
    breaks = 1:10,
    name = "Height of Waves (feet)"
  ) +
  labs(
    y = "Awesomeness of Child's Day\n(Scale 1-10)"
  ) +
  scale_color_discrete(
    name = "Treatment",
    labels = c("Did not surf","Surfed")
  )
ggsave("figures/dr_observedErrorWeighted.pdf", height = 3, width = 5)

# Note weighted average error for slides
observed_error |>
  summarize(weighted_average_error = weighted.mean(fitted - y, w = weight))


# Calculate true ATT
att <- data |>
  filter(a) |>
  summarize(att = mean(y1 - y0)) |>
  print()

# Calculate model-based estimate
treated_predicted <- data |>
  filter(a) |>
  mutate(
    yhat0 = predict(fit0, newdata = data |> filter(a))
  )
att_outcome_model <- treated_predicted |>
  summarize(estimate = mean(y - yhat0)) |>
  print()

# Calculate errors
true_errors <- data |>
  mutate(yhat0 = predict(fit0, newdata = data)) |>
  mutate(error = yhat0 - y0)

true_errors |>
  group_by(a) |>
  summarize(mse0 = mean((yhat0 - y0) ^ 2))

# Calculate a weighted MSE
fit_a <- glm(a ~ x, data = data)
true_errors_weighted <- true_errors |>
  mutate(
    propensity_score = predict(fit_a),
    weight = case_when(
      a == 1 ~ 1,
      a == 0 ~ propensity_score / (1 - propensity_score)
    )
  )
true_errors_weighted |>
  group_by(a) |>
  summarize(weighted_mse = weighted.mean(
    (yhat0 - y0) ^ 2,
    w = weight
  ))

# Calculate a weighted average error
weighted_average_error <- true_errors_weighted |>
  group_by(a) |>
  summarize(weighted_mean_error = weighted.mean(
    yhat0 - y0,
    w = weight
  ))

# Calculate a corrected estimate
att_outcome_model |>
  mutate(
    correction = weighted_average_error |>
      filter(!a) |>
      pull(weighted_mean_error)
  ) |>
  mutate(corrected = estimate + correction)

att
