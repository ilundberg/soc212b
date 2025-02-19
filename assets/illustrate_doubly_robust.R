
# X -> Y should interact with A and Z
# +1 when A = 1 and Z = 1
# 0 otherwise

One pattern when Z = 0


data <- tibble(id = 1:1e2) |>
  mutate(
    x = runif(n()),
    z = rbinom(n(),1,.5),
    a = rbinom(n(), 1, plogis(-.5 + x + z)),
    y = x * z * a
  )

data |>
  ggplot(aes(x = x, y = y, color = a)) +
  geom_point() +
  facet_wrap(~z)

# Would be easier if Y1 were constant and Y0 had pattern.
# Since estimating ATT requires predicting Y0.
data <- tibble(id = 1:1e2) |>
  mutate(
    x = runif(n()),
    z = rbinom(n(),1,.5),
    a = rbinom(n(), 1, plogis(-.5 + x + z)),
    y1 = 1,
    y0 = x * z,
    y = case_when(
      a == 1 ~ y1,
      a == 0 ~ y0
    )
  )
data |>
  ggplot(aes(x = x, y = y, color = factor(a))) +
  geom_point() +
  facet_wrap(~z)

# Y is ice cream sales
# A is raining
# Z is temperature

# On a rainy day, people only buy ice cream if the temperature is high
# On a sunny day, people buy ice cream regardless of the temperature
# Sunny days tend to be warmer.

# Temperature is a confounder.
# Rain is the treatment. (but temperature affects whether raining? not good)
# Ice cream sales is outcome.

# Ice cream sales increase with temperature only when it is raining.

data <- tibble(x = runif(1e4, -1, 1)) |>
  mutate(a = as.logical(rbinom(n(), 1, plogis(2 * x)))) |>
  mutate(
    y0 = .5 - .5 * x ^ 2,
    y1 = 1,
    y = case_when(
      !a ~ y0,
      a ~ y1
    )
  )

data |>
  ggplot(aes(x = x, y = y, color = a)) +
  geom_line()


# Y0: Average happiness without a pool
# Y1: Average happiness with a pool
# X: Temperature

# People who live in warm climates more likely to have a pool



data |>
  ggplot(aes(x = x, fill = a)) +
  geom_density(alpha = .2)

fit_wrong <- lm(y ~ x + a, data = data)

fitted <- data |>
  mutate(y = predict(fit_wrong), set = "Fitted Values")

data |>
  mutate(set = "True Values") |>
  bind_rows(fitted) |>
  ggplot(aes(x = x, y = y, color = a, linetype = set)) +
  geom_line()

# Just focus on untreated

to_predict_even_space <- tibble(x = seq(-1,1,.01))
fit0 <- lm(y ~ x, data = data |> filter(!a))

fitted <- to_predict_even_space |>
  mutate(y = predict(
    lm(y ~ x, data = data |> filter(!a)),
    newdata = to_predict_even_space
  )) |>
  mutate(set = "Best-fit line\nin observed data")

data |>
  mutate(set = "True values") |>
  filter(!a) |>
  bind_rows(fitted) |>
  ggplot(aes(x = x, y = y, linetype = fct_rev(set))) +
  geom_line()

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

# Idea: Flip quadratic upside down so predicts close to zero effect when wrong




data |>
  filter(!a) |>
  mutate(fitted = predict(lm(y ~ x, data = data |> filter(!a)))) |>
  ggplot(aes(x = x, y = y)) +
  geom_line() +
  geom_line(aes(y = fitted), linetype = "dashed")


# X is childhood family income
# A is college completion




# 1 continuous X
# A = 1 more common when X large
# Nonlinear response to X?

simulated <- tibble(x = runif(1e3)) |>
  mutate(
    a = as.logical(rbinom(n(), 1, x / 10)),
    y0 = runif(n(), 0, .02),
    y1 = sqrt(1 - (x - 1)^2),
    y = ifelse(a, y1, y0)
  )
simulated |>
  ggplot(aes(x = x, y = y, color = a)) +
  geom_line() +
  geom_point()

treated_cases <- simulated |> filter(a == TRUE)
treated_cases_as_if_untreated <- treated_cases |>
  mutate(a = FALSE)

truth <- treated_cases |>
  summarize(att = mean(y1 - y0)) |>
  print()

outcome_model_wrong <- lm(y ~ x*a, data = simulated)
# Visualize wrong outcome model
simulated |>
  ggplot(aes(x = x, y = y, color = a)) +
  geom_line() +
  geom_line(data = simulated |> mutate(y = predict(outcome_model_wrong)),
            linetype = "dashed")

estimate_wrong_outcome_model <- treated_cases |>
  mutate(
    yhat1 = predict(outcome_model_wrong, newdata = treated_cases),
    yhat0 = predict(outcome_model_wrong, newdata = treated_cases_as_if_untreated)
  ) |>
  summarize(estimate_wrong_outcome_model = mean(yhat1 - yhat0)) |>
  print()


# TRY MORE DISCRETE VERSION

simulated <- tibble(
  x = seq(.1,.9, .1),
  a = c(F,F,T,F,T,F,T,T,T)
) |>
  mutate(y0 = 0,
         y1 = sqrt(1 - (x - 1)^2),
         y = ifelse(a, y1, y0))

simulated |>
  ggplot(aes(x = x, color = a, y = y)) +
  geom_point()




# Currently: 

# I need these to be confounders. Needs to be the case that
# more home runs happen when more seats are filled

data |>
  mutate(
    a = factor(a, labels = c("Treatment = 0:\nFly ball","Treatment = 1:\nHome run!")),
    z = factor(z, labels = c("Z: Away team is batting","Z: Home team is batting"))
  ) |>
  ggplot(aes(x = x, y = y, color = a)) +
  geom_point() +
  facet_grid(
    z ~ a
  ) +
  xlab("X: Proportion of Seats Filled") +
  ylab("Y: Noise-o-Meter Volume")


Treatment is taking soc 114
Outcome is confidence in data science skills

Taking soc 114 only affects your skills if you do the homework
- problem: mediator instead of confounder

Doing homework affects data science skills only
- if you take soc 114
- if you ask questions on Piazza

Can I use a physical example, or a crowd or something?

Z: Dodgers are the home team
X: How full stadium is
A: Dodgers hit a home run

What is the effect of Dodgers hitting a home run on the noise meter?







# Fit model on untreated units

# Predict for treated units