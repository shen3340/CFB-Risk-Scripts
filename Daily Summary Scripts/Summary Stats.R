library(httr)
library(jsonlite)
library(tidyverse)
library(purrr)
library(gt)
library(ggthemes)
season <- 5
day <- 24
milestones <- c(50, 100, 150, 200, 250)
filename <- paste0("Daily Summary Scripts/Images/Luckiest Territories.png")

# Fetch and parse JSON data
fetch_data <- function(url, query = list()) {
  response <- GET(url, query = query)
  stop_for_status(response)
  content(response, "text", encoding = "UTF-8") %>%
    fromJSON(flatten = TRUE) %>%
    as_tibble()
}

# Fetch teams by season
fetch_teams <- function(season) {
  fetch_data("https://collegefootballrisk.com/api/teams") %>%
    mutate(seasons = map_dbl(seasons, ~ max(unlist(.x)))) %>%
    filter(seasons == season)
}

# Fetch and filter player data for milestones
fetch_players <- function(teams, season, day, milestones) {
  teams$name %>%
    map_dfr(function(team) {
      players_url <- paste0("https://collegefootballrisk.com/api/players?team=", team)
      players_data <- safely(fetch_data)(players_url)$result
      if (!is.null(players_data) && "turnsPlayed" %in% names(players_data)) {
        players_data %>%
          filter(season == season, day == day, turnsPlayed %in% milestones)
      } else {
        tibble()
      }
    })
}

# Generate milestone messages
generate_milestone_messages <- function(players, milestones) {
  milestones %>%
    walk(function(milestone) {
      qualifying_players <- players %>%
        filter(turnsPlayed == milestone) %>%
        mutate(player = str_remove(player, "\\$0$")) %>% # Remove $0 at the end of player names
        pull(player)
      
      message <- paste("Congratulations to the following players who have played for", 
                       milestone, "turns:", 
                       if (length(qualifying_players) > 0) paste(qualifying_players, collapse = ", ") else "")
      print(message)
    })
}


# Fetch odds and validate structure
is_valid_odds <- function(odds) {
  !is.null(odds) &&
    is.data.frame(odds) &&
    all(c("territory", "winner", "chance") %in% colnames(odds))
}

fetch_team_odds <- function(team, season, day) {
  # Try fetching data and handle errors
  response <- safely(GET)("https://collegefootballrisk.com/api/team/odds", 
                          query = list(team = team, season = season, day = day))
  
  if (!is.null(response$result) && http_error(response$result)) {
    return(tibble(territory = character(0), winner = character(0), chance = numeric(0), team = character(0)))
  }
  
  odds <- safely(content)(response$result, "text", encoding = "UTF-8")$result
  odds <- safely(fromJSON)(odds)$result
  
  if (is_valid_odds(odds)) {
    odds %>%
      select(territory, winner, chance) %>%
      mutate(team = team)
  } else {
    tibble(territory = character(0), winner = character(0), chance = numeric(0), team = character(0))
  }
}

# Fetch odds for all teams
fetch_all_odds <- function(teams, season, day) {
  map_dfr(teams$name, ~fetch_team_odds(.x, season, day))
}

# Find the luckiest territories
get_luckiest_territories <- function(data, top_n = 5) {
  data %>%
    filter(team == winner, chance <= 0.5) %>%
    group_by(territory) %>%
    mutate(min_chance = min(chance)) %>%
    filter(chance == min_chance) %>%
    ungroup() %>%
    arrange(chance) %>%
    head(top_n)
}

# Main workflow
teams_data <- fetch_teams(season)
odds_data <- fetch_all_odds(teams_data, season, day)
luckiest_territories <- get_luckiest_territories(odds_data, top_n = 5)

# Get the number of luckiest territories to adjust the title
num_luckiest <- nrow(luckiest_territories)

# Generate the table
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
    title = paste0("Top ", num_luckiest, " Luckiest Territories"),
    subtitle = paste0("Season ", season, ", Day ", day)
  ) |> 
  gtsave(filename, expand = 10)

# Fetch and process player milestones
players_meeting_criteria <- fetch_players(teams_data, season, day, milestones)
generate_milestone_messages(players_meeting_criteria, milestones)
