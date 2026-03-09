# Analyze and Visualize Gambling Data ----
# JOUR 377 - Data Journalism

## Load packages and data ----
library(tidyverse)
library(scales)
library(ggthemes)

gambling_v1 <- read_csv("data/gambling_v1.csv")
# v1 is our data related to number of bets/total monies by each sport
gambling_v2 <- read_csv("data/gambling_v2.csv")
# v2 is our data split between type 1 (outcome) and type 2 (prop bet) wagers

## Number of Wagers ----

### Total number of wagers per year
bets_per_year <- gambling_v1 |> 
  filter(detail_type == "Wager Details" & location_type == "Total") |> 
  summarize(
    n_bets = sum(baseball, basketball, boxing_mma, football, golf, hockey, soccer,
                 tennis, parlay, other_sport, motor_race_event, motor_race_parlay,
                 other_event, other_event_parlay),
    .by = year
  ) |> 
  ggplot(aes(year, n_bets)) +
  geom_line() +
  geom_point(size = 2, color = "red") +
  scale_y_continuous(labels = label_number(scale = 1e-6, suffix = " million")) +
  theme_minimal() +
  labs(
    title = "Sports wagering has skyrocketed over the last five years",
    subtitle = "Number of total bets plateaued in 2025 after rapid growth between 2021-2024",
    x = NULL,
    y = "Number of bets",
    caption = "Source: Illinois Gaming Board\n Graphic by Braedon Olsen"
  )

### Stacked barchart showing number of in-person vs online bets per year ----
bets_per_year_per_type <- gambling_v1 |> 
  filter(detail_type == "Wager Details",
         location_type %in% c("In-Person Wagering", "Online Wagering")) |> 
  summarize(
    n_bets = sum(rowSums(across(c(
      baseball, basketball, boxing_mma, football, golf, hockey, soccer,
      tennis, parlay, other_sport, motor_race_event, motor_race_parlay,
      other_event, other_event_parlay
    )), na.rm = TRUE)),
    .by = c(year, location_type)
  ) |> 
  ggplot(aes(year, n_bets, fill = location_type)) +
  geom_col() +
  scale_y_continuous(labels = label_number(scale = 1e-6, suffix = " million")) +
  theme_minimal() +
  labs(
    title = "Online wagering dominates sports betting",
    subtitle = "Total number of wagers per year, split between online and in-person betting",
    x = NULL,
    y = "Number of bets placed",
    fill = NULL,
    caption = "Source: Illinois Gaming Board\n Graphic by Braedon Olsen"
  )







## Save out plots ----
ggsave(filename = "plots/bets_per_year.png", plot = bets_per_year)
ggsave(filename = "plots/bets_per_year_per_type.png", plot = bets_per_year_per_type)














