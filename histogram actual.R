library(httr)
library(dplyr)
library(purrr)

base_url <- "https://collegefootballrisk.com/api"
season <- 5
team_name <- "March"  # Replace with your team
day <- 13

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
        
        # Convert hex to RGBA
        primary_rgb <- hex_to_rgb(primary_color)
        secondary_rgb <- hex_to_rgb(secondary_color)
        
        return(list(primary = primary_rgb, secondary = secondary_rgb))
      } else {
        stop("No color information found for the team.")
      }
    }
  }
  
  stop("Team not found.")
}

# Function to convert hex to RGB
hex_to_rgb <- function(hex_color) {
  # Remove the hash symbol if it exists
  hex_color <- gsub("^#", "", hex_color)
  
  # Extract RGB values from the hex string
  r <- strtoi(substr(hex_color, 1, 2), 16L)
  g <- strtoi(substr(hex_color, 3, 4), 16L)
  b <- strtoi(substr(hex_color, 5, 6), 16L)
  
  # Return as a vector of RGB values
  return(c(r, g, b))
}

rgb_color <- get_team_colors(team_name)



# Helper function to get data for one team
get_team_data <- function(team_name, day, season) {
  team_odds_url <- paste0(base_url, "/team/odds")
  response <- GET(team_odds_url, query = list(season = season, day = day, team = team_name))
  team_data <- content(response, "parsed")
  
  if (length(team_data) == 0) {
    stop("No data found for the team.")
  }
  return(team_data)
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
  
  # Calculate indices for chart annotations
  indices <- seq_along(vals) - 1  # 0-based indexing
  mu <- sum(vals * indices)  # Weighted average
  sigma <- sqrt(sum(vals * (indices - mu)^2))  # Weighted variance
  three_sigma <- 3 * sigma
  delta_sigma <- ifelse(sigma == 0, 0, (actual - mu) / sigma)
  prob <- 100 * vals[actual]
  
  # Return the results
  list(
    actual = actual,
    expected = expected,
    mu = mu,
    sigma = sigma,
    three_sigma = three_sigma,
    delta_sigma = delta_sigma,
    prob = prob
  )
}

# Main Execution
team_data <- get_team_data(team_name, day, season)
stats <- calculate_statistics(team_data)
