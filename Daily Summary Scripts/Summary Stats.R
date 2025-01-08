library(httr)
library(jsonlite)
library(tidyverse)
library(gt)

season <- 5
day <- 6
filename <- paste0("Daily Summary Scripts/Images/5 Luckiest Territories Season ", season, ", Day ", day, ".png")

# Function to fetch all teams and filter by season
fetch_all_teams <- function(season) {
  teams <- GET("https://collegefootballrisk.com/api/teams") %>%
    content("text", encoding = "UTF-8") %>%
    fromJSON()
  
  teams %>%
    filter(map_lgl(seasons, ~season %in% .)) %>%
    pull(name)
}

# Check if odds data is valid and structured correctly
is_valid_odds <- function(odds) {
  !is.null(odds) &&
    is.data.frame(odds) &&
    all(c("territory", "winner", "chance") %in% colnames(odds))
}

# Fetch odds for a specific team and validate structure
fetch_team_odds <- function(team, season, day) {
  odds <- GET("https://collegefootballrisk.com/api/team/odds", 
              query = list(team = team, season = season, day = day)) %>%
    content("text", encoding = "UTF-8") %>%
    fromJSON()
  
  if (is_valid_odds(odds)) {
    odds %>%
      select(territory, winner, chance) %>%
      mutate(team = team)
  } else {
    tibble(territory = character(0), winner = character(0), chance = numeric(0), team = character(0))
  }
}

# Fetch data for all teams and handle invalid responses
fetch_all_odds <- function(teams, season, day) {
  map_dfr(teams, ~fetch_team_odds(.x, season, day))
}

# Process data to find the luckiest outcomes
get_luckiest_territories <- function(data, top_n = 5) {
  data %>%
    filter(team == winner) %>%
    group_by(territory) %>%
    mutate(min_chance = min(chance)) %>%
    filter(chance == min_chance) %>%
    ungroup() %>%
    arrange(chance) %>%
    head(top_n)
}

# Main workflow
all_teams <- fetch_all_teams(season)
odds_data <- fetch_all_odds(all_teams, season, day)
luckiest_territories <- get_luckiest_territories(odds_data, top_n = 5)

luckiest_territories %>%
  select(territory, winner, chance) %>%
  gt() %>%
  fmt_percent(columns = chance, decimals = 2) %>%
  cols_label(
    territory = "Territory",
    winner = "Winner",
    chance = "Winning Odds (%)"
  ) %>%
  tab_header(
    title = "Top 5 Luckiest Territories",
    subtitle = paste0("Season ", season, ", Day ", day)
  ) |> 
  gtsave(filename, expand = 10)
