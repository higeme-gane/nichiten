rm(list = ls())
library(conflicted)
library(tidyverse)
library(psych)
conflicts_prefer(dplyr::filter)

threshold_rate <- 0.25
fisher_alt <-  "greater"

df <- read_csv("nichiten_loto_2025.csv", show_col_types = FALSE)
head(arrange(.data = df, value))
head(arrange(.data = df, desc(value)))
df_describe <- describe(df$value)
df_describe

n <- df_describe$n 
n_expected <- df_describe$mean
n_applicants <- n * n_expected

piv_value <- df |> 
  summarise(n_degree = n(), .by = "value") |> 
  arrange(value) |> 
  mutate(n_cumsum = cumsum(n_degree)) |> 
  mutate(n_cum_pct = n_cumsum / n) |> 
  mutate(n_people = value * n_degree) |> 
  mutate(people_cumsum = cumsum(n_people)) |> 
  mutate(people_cum_pct = people_cumsum / n_applicants) 
print(piv_value, n = 13)

threshold <- piv_value |>
  filter(n_cum_pct < threshold_rate) |>
  arrange(desc(n_cum_pct)) |>
  slice(1) |>
  pull(value)

g <- ggplot(df, aes(x = value))
g <- g + geom_rect(xmin = 0, xmax = threshold, ymin = 0, ymax = Inf, fill = "pink", alpha = 0.5)
g <- g + geom_histogram(binwidth = 1, fill = "blue", color = "black")
g <- g + geom_vline(xintercept = n_expected, color = "purple", linetype = "dashed")
g <- g + labs(title = "Histogram of Values with Mean", x = "Value", y = "Frequency")
plot(g)
# ggsave("histgram.png", plot = g,
#        width = 2039,
#        height = 1447,
#        units = c("px"))

df_long <- df |> 
  mutate(num_pad = str_pad(num, width = 3, side = c("left"), pad = "0")) |> 
  mutate(range1 = str_sub(num_pad, 1, 1),
         range2 = str_sub(num_pad, 2, 2),
         range3 = str_sub(num_pad, 3, 3)) |> 
  mutate(across(c(range1, range2, range3), as.numeric)) |> 
  mutate(same_2 = if_else(range1 != range2 & range2 != range3 & range1 != range3, 0, 1),
         same_100_10 = if_else(range1 == range2, 1, 0),
         same_10_1 = if_else(range2 == range3, 1, 0),
         same_100_1 = if_else(range1 == range3, 1, 0)) |> 
  mutate(rates = if_else(value <= threshold, "unpopular", "not_unpopular"))

piv_popular <- df_long |> 
  summarise(value = n(), .by = "rates")
unpopular_rate_all <- piv_popular |>
  filter(rates == "unpopular") |>
  pull(value) / n

mat_popular <- piv_popular |> 
  mutate(range = "all")

df_select <- df_long |> 
  select(-num, -value, -num_pad) |> 
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

piv_range_all <- df_select |> 
  summarise(value = n(), .by = c("name", "item")) |> 
  mutate(rates = "all") |> 
  rename(range = name) |> 
  bind_rows(piv_range) |> 
  pivot_wider(names_from = rates) |> 
  mutate(unpopular_rate = unpopular / all)
piv_range_pc <- piv_range_all |> 
  select(range, item, unpopular_rate)

# 例として、100の位が0の場合のfisher.test()
df_work <- piv_range |> 
  filter(range == vec_range[1]) |> 
  filter(item == 0)  |> 
  select(-item) |> 
  bind_rows(mat_popular) |> 
  pivot_wider(names_from = range) |> 
  mutate(across(everything(), ~ replace_na(.x, 0))) |> 
  mutate(biased = all - !!sym(vec_range[1])) |> 
  select(-all) |> 
  pivot_longer(cols = -rates, names_to = "range") |> 
  pivot_wider(names_from = rates) |> 
  arrange(desc(range)) |> 
  select(unpopular, not_unpopular) |> 
  as.matrix()
fisher.test(df_work, alternative = fisher_alt, conf.level = 0.95)

# 同じ数が2回以上使われている場合のfisher.test()
df_work2 <- piv_range |> 
  filter(range == vec_range[4]) |> 
  filter(item == 1)  |> 
  select(-item) |> 
  bind_rows(mat_popular) |> 
  pivot_wider(names_from = range) |> 
  mutate(across(everything(), ~ replace_na(.x, 0))) |> 
  mutate(biased = all - !!sym(vec_range[4])) |> 
  select(-all) |> 
  pivot_longer(cols = -rates, names_to = "range") |> 
  pivot_wider(names_from = rates) |> 
  arrange(desc(range)) |> 
  select(unpopular, not_unpopular) |> 
  as.matrix()
fisher.test(df_work2, alternative = fisher_alt, conf.level = 0.95)

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
    select(-all) |> 
    pivot_longer(cols = -rates, names_to = "range") |> 
    pivot_wider(names_from = rates) |> 
    arrange(desc(range)) |> 
    select(unpopular, not_unpopular) |> 
    as.matrix()
    # select(!!sym(fn_range), biased) |> 
  fisher_p_value <- fisher.test(mat_range, alternative = fisher_alt)$p.value
  dplyr::tibble(range = fn_range, item = fn_item, p_value = fisher_p_value)
  }) |> list_rbind()
}) |> list_rbind() |> 
  mutate(sig_dif = if_else(p_value < 0.05, 1, 0)) |> 
  left_join(piv_range_pc, by = c("range", "item")) |> 
  mutate(unpopular_code = if_else(unpopular_rate > unpopular_rate_all, 1, 0))
# write_csv(df_p_value, "df_p_value.csv")

df_unpopular <- df_p_value |> 
  filter(sig_dif == 1 & unpopular_code == 1) |> 
  select(-sig_dif, -unpopular_code)