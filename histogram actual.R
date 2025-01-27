library(httr)
library(tidyverse)
library(purrr)

base_url <- "https://collegefootballrisk.com/api"
season <- 5
team_name <- "March"  # Replace with your team
day <- 22

# Function to get team colors
get_team_colors <- function(team_name) {
  # Fetch the list of all teams
  teams_url <- paste0(base_url, "/teams")
  teams_response <- GET(teams_url)
  teams_data <- content(teams_response, "parsed")
  
  # Find the team by name and extract the primary and secondary colors
  for (team in teams_data) {
    if (team$name == team_name) {
      if ("colors" %in% names(team) && "primary" %in% names(team$colors) && "secondary" %in% names(team$colors)) {
        primary_color <- team$colors$primary
        secondary_color <- team$colors$secondary
        
        return(list(primary = primary_color, secondary = secondary_color))
      } else {
        stop("No color information found for the team.")
      }
    }
  }
  
  stop("Team not found.")
}


rgb_color <- get_team_colors(team_name)


# Function to get team data
get_team_data <- function(team_name, day, season) {
  team_odds_url <- paste0(base_url, "/team/odds")
  response <- GET(team_odds_url, query = list(season = season, day = day, team = team_name))
  team_data <- content(response, "parsed")
  
  if (length(team_data) == 0) {
    stop("No data found for the team.")
  }
  return(team_data)
}

# Function to get leaderboard data for a specific day
get_leaderboard_data <- function(season, day) {
  leaderboard_url <- paste0(base_url, "/stats/leaderboard")
  response <- GET(leaderboard_url, query = list(season = season, day = day))
  leaderboard_data <- content(response, "parsed")
  
  if (length(leaderboard_data) == 0) {
    stop("No leaderboard data found.")
  }
  return(leaderboard_data)
}
leaderboard_day_prev <- get_leaderboard_data(season, day - 1)
territories_day_prev<- NA  # Default value in case the team isn't found

for (entry in leaderboard_day_prev) {
  if (entry$name == team_name) {
    territories_day_prev <- entry$territoryCount
    break  # Stop once we find the team
  }
}

# Check if we found the team
if (is.na(territories_day_prev)) {
  stop("Team not found in the leaderboard data for day 4.")
}

# Function to calculate statistics
calculate_statistics <- function(team_data) {
  # Extract team and territory powers
  team_powers <- sapply(team_data, function(t) t$teamPower)
  territory_powers <- sapply(team_data, function(t) t$territoryPower)
  
  # Compute odds
  odds <- ifelse(territory_powers > 0, team_powers / territory_powers, 1)
  
  # Actual wins
  actual <- sum(sapply(team_data, function(t) t$winner == team_name))
  
  # Expected wins
  expected <- sum(odds)
  
  # Convolution to calculate PDF
  vals <- 1
  for (k in odds) {
    vals <- convolve(vals, rev(c(1 - k, k)), type = "open")
  }
  
  # Normalize vals to ensure it represents a valid probability distribution
  vals <- vals / sum(vals)
  
  # Return the results
  list(
    actual = actual,
    expected = expected,
    vals = vals
  )
}

# Main Execution
team_data <- get_team_data(team_name, day, season)
stats <- calculate_statistics(team_data)
stats$expected <- round(stats$expected, 2)
delta <- stats$actual - territories_day_prev
territories_oe <- stats$actual - stats$expected

# Create a data frame for plotting
probability_df <- data.frame(
  Territories = seq_along(stats$vals) - 1,  # 0-based indexing
  Probability = stats$vals * 100  # Convert to percentage
)

if (territories_oe < 0) {
  act_color = "#781b0e"
} else {
  act_color = "#3b8750"
}
# Plot the histogram
ggplot(probability_df, aes(x = Territories, y = Probability)) +
  geom_bar(stat = "identity", fill = rgb_color$primary, color = rgb_color$secondary) +
  geom_vline(xintercept = territories_day_prev, color = "#ffb521", linetype = "dashed", size = 1) + 
  geom_vline(xintercept = stats$actual, color = act_color, linetype = "dashed", size = 1) + 
  geom_vline(xintercept = stats$expected, color = "#081840", linetype = "dashed", size = 1) + 
  labs(
    title = paste("Number of Territories Histogram:", team_name),
    subtitle = paste("<i>Expected:</i> ", stats$expected, ", <i>Actual:</i> ", stats$actual, ", <i>&#x0394;</i>Territories=", delta),
    x = "Number of Territories Won",
    y = "Percent Chance to Win N Territories (%)"
  ) +
  theme_hc() +
  theme(
    plot.subtitle = ggtext::element_markdown(hjust = 0.5),
    plot.title = element_text(hjust = 0.5)
  )

