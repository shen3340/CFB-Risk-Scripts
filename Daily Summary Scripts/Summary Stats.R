library(httr)
library(jsonlite)
library(tidyverse)
library(gt)
season <- 5
day <- 4

# Function to fetch all teams and filter by season
fetch_all_teams <- function(season) {
  teams <- GET("https://collegefootballrisk.com/api/teams") %>%
    content("text", encoding = "UTF-8") %>%
    fromJSON()
  
  # Filter teams that are in the current season (season should be in the 'seasons' list)
  teams %>%
    filter(map_lgl(seasons, ~season %in% .)) %>%  # Check if the current season is in the 'seasons' field
    pull(name)  # Return only the names of the teams
}

# Fetch odds for a specific team
fetch_team_odds <- function(team, season, day) {
  GET("https://collegefootballrisk.com/api/team/odds", 
      query = list(team = team, season = season, day = day)) %>%
    content("text", encoding = "UTF-8") %>%
    fromJSON() %>%
    select(territory, winner, chance) %>%
    mutate(team = team)  # Add team column to identify which team the data belongs to
}

# Fetch data for all teams
fetch_all_odds <- function(teams, season, day) {
  map_dfr(teams, ~fetch_team_odds(.x, season, day))
}

# Process data to find the luckiest outcomes
get_luckiest_territories <- function(data, top_n = 5) {
  data %>%
    # Filter only rows where the team is the winner
    filter(team == winner) %>%
    group_by(territory) %>%
    mutate(min_chance = min(chance)) %>%  # Find the lowest chance for each territory
    filter(chance == min_chance) %>%      # Keep only the luckiest outcome
    ungroup() %>%
    arrange(chance) %>%                   # Sort by luckiest (lowest chance)
    head(top_n)                           # Select top N luckiest territories
}

# Fetch all teams in the current season
all_teams <- fetch_all_teams(season)

# Fetch odds data for all teams
odds_data <- fetch_all_odds(all_teams, season, day)

# Identify the luckiest territories
luckiest_territories <- get_luckiest_territories(odds_data, top_n = 5)

# Generate table
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
  )
