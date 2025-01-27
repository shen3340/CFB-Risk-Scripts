library(httr)
library(tidyverse)
library(ggthemes)

base_url <- "https://collegefootballrisk.com/api"

# Main Execution
season <- 5
team_name <- "July"
day <- 22



# Helper to fetch API data
fetch_api_data <- function(endpoint, query = list()) {
  response <- GET(paste0(base_url, endpoint), query = query)
  content(response, "parsed")
}

# Get team colors
get_team_colors <- function(team_name) {
  teams_data <- fetch_api_data("/teams")
  team <- teams_data %>% keep(~ .x$name == team_name) %>% first()
  if (!is.null(team) && "colors" %in% names(team)) {
    return(team$colors[c("primary", "secondary")])
  }
  stop("Team or color information not found.")
}
team_colors <- get_team_colors(team_name)

# Get data
get_team_data <- function(team_name, day, season) {
  fetch_api_data("/team/odds", list(season = season, day = day, team = team_name))
}
team_data <- get_team_data(team_name, day, season)

get_leaderboard_data <- function(season, day, team_name) {
  leaderboard <- fetch_api_data("/stats/leaderboard", list(season = season, day = day))
  entry <- leaderboard %>% keep(~ .x$name == team_name) %>% first()
  if (!is.null(entry)) return(entry$territoryCount)
  stop("Team not found in leaderboard.")
}
territories_day_prev <- get_leaderboard_data(season, day - 1, team_name)

# Calculate statistics
calculate_statistics <- function(team_data, team_name) {
  odds <- map_dbl(team_data, ~ ifelse(.x$territoryPower > 0, .x$teamPower / .x$territoryPower, 1))
  actual <- sum(map_lgl(team_data, ~ .x$winner == team_name))
  expected <- sum(odds)
  
  vals <- reduce(odds, ~ convolve(.x, rev(c(1 - .y, .y)), type = "open"), .init = 1)
  vals <- vals / sum(vals)  # Normalize
  
  list(actual = actual, expected = expected, vals = vals)
}
stats <- calculate_statistics(team_data, team_name)
delta <- stats$actual - territories_day_prev
territories_oe <- stats$actual - stats$expected
probability_df <- tibble(
  Territories = seq_along(stats$vals) - 1,
  Probability = stats$vals * 100
)

legend_data <- tibble(
  xintercept = c(territories_day_prev, stats$actual, stats$expected),
  label = c("Prev Num. Territories", "Actual Territories", "Expected Value"),
  color = c("#ffb521", ifelse(territories_oe < 0, "#781b0e", "#3b8750"), "#081840"),
  linetype = "dashed"
)

x_range <- max(probability_df$Territories) - min(probability_df$Territories)

if (x_range > 50) {
  increment <- 10
} else if (x_range > 20) {
  increment <- 5
} else if (x_range > 5){
  increment <- 2
} else {
  increment <- 1
}


# Plot histogram
plot_histogram <- function(probability_df, legend_data, colors, stats, team_name, delta) {
  ggplot(probability_df, aes(x = Territories, y = Probability)) +
    geom_bar(stat = "identity", fill = colors$primary, color = colors$secondary) +
    geom_vline(data = legend_data, aes(xintercept = xintercept, color = label, linetype = label), linewidth = 1) +
    scale_color_manual(values = setNames(legend_data$color, legend_data$label)) +
    scale_linetype_manual(values = setNames(legend_data$linetype, legend_data$label)) +
    scale_x_continuous(
      breaks = seq(min(probability_df$Territories), max(probability_df$Territories), by = increment)
    ) + 
    labs(
      title = paste("Number of Territories Histogram:", team_name),
      subtitle = paste("<i>Expected:</i>", round(stats$expected, 2), ", <i>Actual:</i>", stats$actual, ", <i>Δ Territories=</i>", delta),
      x = "Number of Territories Won",
      y = "Percent Chance to Win N Territories (%)"
    ) +
    theme_hc() +
    theme(
      plot.subtitle = ggtext::element_markdown(hjust = 0.5),
      plot.title = element_text(hjust = 0.5),
      legend.position = "inside",
      legend.title = element_blank()
    )
}

plot_histogram(probability_df, legend_data, team_colors, stats, team_name, delta)

