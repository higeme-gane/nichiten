rm(list = ls())
library(conflicted)
library(tidyverse)
library(openxlsx)
library(psych)
conflicts_prefer(dplyr::filter)

popular_rate <- 3
unpopular_rate <- -3

df_long <- read_csv("nichiten_loto_2025.csv") |> 
  mutate(num_pad = str_pad(num, width = 3, side = c("left"), pad = "0")) |> 
  mutate(range1 = str_sub(num_pad, 1, 1),
         range2 = str_sub(num_pad, 2, 2),
         range3 = str_sub(num_pad, 3, 3),
         last_2digits = str_sub(num_pad, 2, 3)) |> 
  mutate(across(c(range1, range2, range3, last_2digits), as.numeric)) |> 
  mutate(same_100_10_1 = if_else(range1 == range2 & range2 == range3, 1, 0),
         same_100_10 = if_else(range1 == range2, 1, 0),
         same_10_1 = if_else(range2 == range3, 1, 0),
         same_100_1 = if_else(range1 == range3, 1, 0),
         same_2 = if_else(range1 != range2 & range2 != range3 & range1 != range3, 0, 1))

n_applicants <- sum(df_long$value)
n <- nrow(df_long) 
n_expected = n_applicants / n
unpopular_boundary <- unpopular_rate * sqrt(n_expected) + n_expected
popular_boundary <- popular_rate * sqrt(n_expected) + n_expected

df_long <- df_long |> 
  mutate(residuals = (value - n_expected) / sqrt(n_expected)) |> 
  mutate(rates = case_when(
    residuals > popular_rate ~ "popular",
    residuals < unpopular_rate ~ "unpopular",
    .default = "standard"
  ))

g <- ggplot(df_long, aes(x = value)) +
  geom_rect(xmin = unpopular_boundary, xmax = popular_boundary, ymin = -Inf, ymax = Inf, fill = "pink", alpha = 0.5) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  geom_vline(xintercept = n_expected, color = "purple", linetype = "dashed") +
  labs(title = "Histogram of Values with Mean", x = "Value", y = "Frequency")
plot(g)

piv_popular <- df_long |> 
  summarise(value = n(), .by = "rates")
mat_popular <- piv_popular |> 
  mutate(range = "all")

df_select <- df_long |> 
  select(-num, -value, -num_pad, -residuals) |> 
  pivot_longer(cols = -rates, values_to = "item")

vec_range <- df_select |> 
  distinct(name) |> 
  pull()

piv_range <- map(vec_range, ~{
  df_select |> 
  filter(name == .x) |> 
  summarise(value = n(), .by = c("rates", "item")) |> 
  mutate(range = .x)
}) |> list_rbind()

df_p_value <- map(vec_range, function(fn_range){
  df_work_range <- piv_range |> 
    filter(range == fn_range)
  vec_item <- df_work_range |> 
    distinct(item) |> 
    pull()
  map(vec_item, function(fn_item){
    mat_range <- df_work_range |> 
      filter(item == fn_item) |> 
    select(-item) |> 
    bind_rows(mat_popular) |> 
    pivot_wider(names_from = range) |> 
    mutate(across(everything(), ~ replace_na(.x, 0))) |> 
    mutate(biased = all - !!sym(fn_range)) |> 
    select(!!sym(fn_range), biased) |> 
    as.matrix()
  fisher_p_value <- fisher.test(mat_range)$p.value
  dplyr::tibble(range = fn_range, item = fn_item, p_value = fisher_p_value)
  }) |> list_rbind()
}) |> list_rbind() |> 
  mutate(sig_dif = if_else(p_value < 0.05, 1, 0))
# write_csv(df_p_value, "df_p_value.csv")

df_wide <- map(vec_range, ~{
    piv_wide <- piv_range |> 
    filter(range == .x) |> 
    select(-range) |> 
    pivot_wider(names_from = rates) |> 
    mutate(across(everything(), ~ replace_na(.x, 0))) |> 
    mutate(range = .x)
}) |> list_rbind() |> 
  select(range, item, unpopular, standard, popular) |> 
  left_join(df_p_value, by = c("range", "item")) |> 
  mutate(rates = case_when(unpopular > popular ~ "unpopular",
                           unpopular < popular ~ "popular",
                           .default = "even"))
# write_csv(df_wide, "wide_p_value.csv")