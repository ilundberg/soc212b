

truth <- tibble(z = F, x = seq(0,1,.001)) |>
  bind_rows(tibble(z = T, x = seq(0,1,.001))) |>
  mutate(mu = z * plogis(10 * (x - .5)))

p <- truth |>
  ggplot(aes(x = x, color = z, y = mu)) +
  geom_line()

simulate <- function(sample_size) {
   truth |>
    slice_sample(n = sample_size, replace = T) |>
    mutate(y = mu + rnorm(n(), sd = .1))
}

sample <- simulate(1000)
p +
  geom_point(data = sample, aes(y = y))

x_split_candidates <- quantile(sample$x, seq(.1,.9,.1))
z_split_candidates <- .5
by_z <- sample |>
  group_by(z) |>
  mutate(yhat = mean(y)) |>
  ungroup() |>
  summarize(sum_squared_error = sum((yhat - y) ^ 2))
by_x <- foreach(x_split = x_split_candidates, .combine = "rbind") %do% {
  sample |>
    mutate(left = x <= x_split) |>
    group_by(left) |>
    mutate(yhat = mean(y)) |>
    ungroup() |>
    summarize(sum_squared_error = sum((yhat - y) ^ 2)) |>
    mutate(x_split = x_split)
}
by_x |>
  ggplot(aes(x = x_split, y = sum_squared_error)) +
  geom_line() +
  geom_hline(yintercept = by_z$sum_squared_error,
             linetype = "dashed")

# Split 2: After splitting by Z, only X remains on which to split
left_side <- sample |> filter(!z)
right_side <- sample |> filter(z)

left_split_candidates <- quantile(left_side$x, seq(.1,.9,.1))
right_split_candidates <- quantile(right_side$x, seq(.1,.9,.1))

left_split_results <- foreach(x_split = left_split_candidates, .combine = "rbind") %do% {
  left_side |>
    mutate(left = x <= x_split) |>
    group_by(z,left) |>
    mutate(yhat = mean(y)) |>
    ungroup() |>
    summarize(sum_squared_error = sum((yhat - y) ^ 2)) |>
    mutate(x_split = x_split)
} |>
  mutate(chosen = sum_squared_error == min(sum_squared_error))

right_split_results <- foreach(x_split = right_split_candidates, .combine = "rbind") %do% {
  right_side |>
    mutate(left = x <= x_split) |>
    group_by(z,left) |>
    mutate(yhat = mean(y)) |>
    ungroup() |>
    summarize(sum_squared_error = sum((yhat - y) ^ 2)) |>
    mutate(x_split = x_split)
} |>
  mutate(chosen = sum_squared_error == min(sum_squared_error))

split2_results <- left_split_results |> mutate(split1 = "Among Z = FALSE") |>
  bind_rows(right_split_results |> mutate(split1 = "Among Z = TRUE"))

split2_results |>
  ggplot(aes(x = x_split, y = sum_squared_error)) +
  geom_line(color = 'gray') +
  geom_point(aes(color = chosen)) +
  scale_color_manual(values = c("gray","blue")) +
  facet_wrap(~split1) +
  theme_bw()

# Visualize the learned tree
split2_results |>
  filter(chosen) |>
  mutate(z = as.logical(str_remove(split1,"Among Z = "))) |>
  select(z, x_split) |>
  right_join(sample, by = join_by(z)) |>
  mutate(x_left = x <= x_split) |>
  group_by(z, x_left) |>
  mutate(yhat = mean(y)) |>
  ggplot(aes(x = x, color = z, y = yhat)) +
  geom_step() +
  theme_bw()

# Continue iterating that process
this_sample <- simulate(1000)
rpart.out <- rpart(y ~ x + z, data = this_sample, control = rpart.control(minsplit = 2, cp = 0, maxdepth = 5))
#rpart.plot(rpart.out)
this_sample |>
  mutate(yhat = predict(rpart.out)) |>
  ggplot(aes(x = x, y = yhat, color = z)) +
  geom_step()


# Performance of rpart over repeated samples
# at maxdepth = 2 (bias) and 5 (variance)
to_predict <- truth |> filter(z & x == .8)
many_estimates <- foreach(rep = 1:10, .combine = "rbind") %do% {
  this_sample <- simulate(1000)
  rpart.out <- rpart(y ~ x + z, data = this_sample, control = rpart.control(minsplit = 2, cp = 0, maxdepth = 5))
  truth |> mutate(yhat = predict(rpart.out, newdata = truth), rep = rep)
}

many_estimates |>
  ggplot(aes(x = x, color = z)) +
  geom_line(aes(y = mu), linetype = "dashed") +
  geom_step(aes(y = yhat, group = interaction(z,rep)), alpha = .5) +
  theme_bw()

#rpart.plot(rpart.out)
this_sample |>
  mutate(yhat = predict(rpart.out)) |>
  ggplot(aes(x = x, y = yhat, color = z)) +
  geom_step()



# Performance of rpart over maxdepth values
estimator <- function(maxdepth) {
  foreach(rep = 1:1000, .combine = "rbind") %do% {
    this_sample <- simulate(100)
    rpart.out <- rpart(y ~ x + z, data = this_sample, control = rpart.control(minsplit = 2, cp = 0, maxdepth = 5))
    to_predict <- tibble(z = T, x = .8)
    to_predict |>
      mutate(yhat = predict(rpart.out, newdata = to_predict),
             maxdepth = maxdepth)
  }
}
results <- foreach(maxdepth_value = 1:4, .combine = "rbind") %do% estimator(maxdepth = maxdepth_value)

results |>
  left_join(truth, by = c("x","z")) |>
  group_by(maxdepth) |>
  summarize(
    bias_sq = mean(yhat - mu),
    variance = var(yhat),
    mse = mean((yhat - mu) ^ 2)
  ) |>
  select(maxdepth, bias_sq, variance) |>
  pivot_longer(cols = c("bias_sq","variance")) |>
  ggplot(aes(x = maxdepth, y = value)) +
  geom_point() +
  facet_wrap(~name, scales = "free")


    variance = var(yhat),
    variance = var(yhat)
    
    mse = mean((y - yhat) ^ 2)) |>
  ggplot(aes(x = maxdepth, y = mse)) +
  geom_point()

sample |>
  mutate(yhat = predict(rpart.out)) |>
  ggplot(aes(x = x, y = yhat, color = z)) +
  geom_step()
  
  
  select(z, x_split) |>
  right
  filter(chosen) |>
  select(x_split) |>
  

right_split_results |>
  ggplot(aes(x = x_split, y = sum_squared_error)) +
  geom_point() +
  geom_line()
  