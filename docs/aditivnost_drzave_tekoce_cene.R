################################################################################
#                 Testing additivity of current prices                         #
################################################################################

library(tidyr)
library(dplyr)
library(eurostat)

################################################################################
#                 Country level aggregation - EA19                             #
################################################################################
# download data
raw10 <- get_eurostat("nama_10_a10",
                    filters = list(
                      geo =  c("EA19",
                               "AT", "BE", "FI", "FR", "DE",
                               "IE", "IT", "LU", "NL", "PT",
                               "ES", "EL", "SI", "CY", "MT",
                               "SK", "EE","LV","LT"),
                      time = 2015:2022,
                      na_item = "B1G", # Value added, gross
                      unit = c("PYP_MEUR")))

# clean dataframe, aggregate overEA19, calculate discrepancies
test_additivity <-  raw10 |>
  select(-freq, -unit, -na_item) |>
  mutate(group = ifelse(geo == "EA19", "orig", "aggr")) |>
  group_by(group, time, nace_r2) |>
  summarise(across(where(is.numeric), \(x) sum(x))) |> # aggregate
  pivot_wider(names_from = c(nace_r2, group),
              values_from = c(values)) |>
  mutate(across(ends_with("_aggr"), # percent difference
                \(x) ((x / get(sub("_aggr$", "_orig", cur_column()))) - 1) *100,
                .names = "{sub('_aggr$', '', .col)}"),
         .keep = "unused")

head(test_additivity)

# plot
library(ggplot2)
library(ggrepel)
df_long <- test_additivity %>%
  pivot_longer(cols = -time, names_to = "variable", values_to = "value")

df_last <- df_long %>%
  group_by(variable) %>%
  slice_max(time, n = 1)

ggplot(df_long, aes(x = time, y = value, color = variable)) +
  geom_line() +
  theme_minimal() +
  labs(title = "CP_MEUR: % difference between published EA19 aggregates and summed from individual country data",
       x = "",
       y = "difference in %") +
  theme(legend.position = "none") +  # Remove the legend
  scale_x_date(limits = c(min(df_long$time), max(df_long$time) + 80))  # Extend x-axis for labels



################################################################################
#                 NACE level aggregations - M + N and M_N                      #
################################################################################
# download data
raw64 <- get_eurostat("nama_10_a64",
                      filters = list(
                        geo =  c("EA19",
                                 "AT", "BE", "FI", "FR", "DE",
                                 "IE", "IT", "LU", "NL", "PT",
                                 "ES", "EL", "SI", "CY", "MT",
                                 "SK", "EE","LV","LT"),
                        time = 2015:2022,
                        na_item = "B1G", # Value added, gross
                        unit = c("CP_MEUR"),
                        nace_r2 = c("M", "N", "M_N")))

# clean dataframe, aggregate overEA19, calculate discrepancies
test_additivity <-  raw64 |>
  select(-freq, -unit, -na_item) |>
  mutate(group = ifelse(nace_r2 == "M_N", "orig", "aggr")) |>
  group_by(group, time, geo) |>
  summarise(across(where(is.numeric), \(x) sum(x))) |> # aggregate
  pivot_wider(names_from = c(geo, group),
              values_from = c(values)) |>
  mutate(across(ends_with("_aggr"), # percent difference
                \(x) ((x / get(sub("_aggr$", "_orig", cur_column()))) - 1) *100,
                .names = "{sub('_aggr$', '', .col)}"),
         .keep = "unused")

head(test_additivity)

# plot
library(ggplot2)
library(ggrepel)
df_long <- test_additivity %>%
  pivot_longer(cols = -time, names_to = "variable", values_to = "value")

df_last <- df_long %>%
  group_by(variable) %>%
  slice_max(time, n = 1)

ggplot(df_long, aes(x = time, y = value, color = variable)) +
  geom_line() +
  geom_text_repel(data = df_last,
                  aes(label = variable),
                  nudge_x = 1,
                  direction = "y",
                  hjust = 0,
                  segment.color = NA) +
  theme_minimal() +
  labs(title = "CP_MEUR: % difference between published M_N aggregate and summed from M + N",
       x = "",
       y = "difference in %") +
  theme(legend.position = "none") +  # Remove the legend
  scale_x_date(limits = c(min(df_long$time), max(df_long$time) + 80))  # Extend x-axis for labels


