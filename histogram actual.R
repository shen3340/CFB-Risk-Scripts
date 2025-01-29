library(httr)
library(tidyverse)
library(ggthemes)
library(ggThemeAssist)

base_url <- "https://collegefootballrisk.com/api"
season <- 5
team_name <- "September"
day <- 24

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
  prob_draw <- ifelse(actual + 1 <= length(vals), 100 * vals[actual + 1], 0)
  
  list(actual = actual, expected = expected, vals = vals, prob_draw = prob_draw)
}
stats <- calculate_statistics(team_data, team_name)
probability_df <- tibble(
  Territories = seq_along(stats$vals) - 1,
  Probability = stats$vals * 100
)
sigma <- sqrt(sum(probability_df$Probability * (probability_df$Territories - stats$expected)^2) / sum(probability_df$Probability))
delta_territories <- stats$actual - territories_day_prev
territories_oe <- stats$actual - stats$expected
if (sigma == 0) {
  delta <- 0
} else {
  delta <- (stats$actual - stats$expected) / sigma
}

# Create a sequence of x-values for the normal distribution curve
x_values <- seq(min(probability_df$Territories), max(probability_df$Territories), length.out = 100)

# Compute normal probability density function (PDF)
y_values <- dnorm(x_values, mean = stats$expected, sd = sigma)

# Scale y-values to fit the histogram (normalize)
y_values <- y_values / max(y_values) * max(probability_df$Probability)

# Create a data frame for the normal curve
normal_curve <- tibble(Territories = x_values, Probability = y_values)


legend_levels <- c("X ~ N(μ,σ)", "Expected Value","Actual Territories" , "Prev Num. Territories")

legend_data <- tibble(
  xintercept = c(NA, territories_day_prev, stats$actual, stats$expected),
  label = c("X ~ N(μ,σ)","Expected Value", "Actual Territories",  "Prev Num. Territories"),
  color = c("gray34", "#081840" , ifelse(territories_oe < 0, "#781b0e", "#3b8750"),  "#ffb521"),
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

y_range <- max(probability_df$Probability)
if (y_range > 50) {
  y_increment <- 10
} else if (y_range > 25) {
  y_increment <- 5
} else if (y_range >20) {
  y_increment <- 2.5
} else if (y_range > 10) {
  y_increment <- 2
} else {
  y_increment <- 1
}
y_max <- ceiling(y_range / y_increment) * y_increment

if (max(probability_df$Territories) < 5) { 
  legend_x <- 0.8  # Move to top-right if bars are near the left
} else { 
  legend_x <- 0.2  # Default to top-left
}

# Plot histogram
plot_histogram <- function(probability_df, legend_data, colors, stats, team_name, delta_territories) {
  box_xmin <- 4
  box_xmax <- 13
  box_ymin <- 16
  box_ymax <- 25
  ggplot(probability_df, aes(x = Territories, y = Probability)) +
    geom_bar(stat = "identity", fill = colors$primary, color = colors$secondary, position = position_identity()) +
    geom_vline(data = legend_data, aes(xintercept = xintercept, 
                                       color = factor(label, levels = legend_levels), 
                                       linetype = factor(label, levels = legend_levels)), 
               linewidth = 1, show.legend = FALSE) +
    geom_hline(data = legend_data, aes(yintercept = 0, 
                                       color = factor(label, levels = legend_levels), 
                                       linetype = factor(label, levels = legend_levels)), 
               linewidth = 1)  +
    geom_line(data = normal_curve, aes(x = Territories, y = Probability), 
              color = "#54585A", linewidth = 1, linetype = "solid") +
    scale_color_manual(values = setNames(legend_data$color, legend_levels)) +
    scale_linetype_manual(values = setNames(legend_data$linetype, legend_levels)) +
    scale_x_continuous(
      breaks = seq(min(probability_df$Territories), max(probability_df$Territories), by = increment),
      limits = c(0, max(probability_df$Territories)),  
      expand = c(0, 0),
      labels = function(x) ifelse(x == 0, "0", as.character(x))  # Only show one "0"
    ) +
    scale_y_continuous(
      breaks = seq(0, y_max, by = y_increment),  # Ensure breaks include the rounded max
      limits = c(0, y_max),  # Extend the upper limit
      expand = c(0, 0),
      labels = function(y) ifelse(y == 0, "0", as.character(y))  # Ensure "0" is visible
    ) +  
    labs(
      title = paste("Number of Territories Histogram:", team_name),
      subtitle = paste("<i>Expected:</i>", round(stats$expected, 2), ", <i>Actual:</i>", stats$actual, ", <i>Δ Territories=</i>", delta_territories),
      x = "Number of Territories Won",
      y = "Percent Chance to Win N Territories (%)"
    ) +
    theme_hc() +
    theme(
      panel.background = element_rect(fill = "gray92", color = NA),
      legend.background = element_rect(fill = "gray94", color = NA),
      plot.subtitle = ggtext::element_markdown(size = 16, hjust = 0.5),
      plot.title = element_text(size = 18, hjust = 0.5),
      legend.position = c(legend_x, .95),  # Moves legend to the top-center
      legend.justification = c(0.5, 1),  # Aligns legend properly at the top
      legend.direction = "vertical", 
      legend.title = element_blank(),
      axis.title = element_text(size = 16),
      axis.text = element_text(size = 14, colour = "black"),
      panel.grid.major = element_line(color = "gray60", linewidth = 0.5, linetype = "dashed"),  # Major gridlines
      panel.grid.major.x = element_line(linetype = "dashed"),
      panel.grid.major.y = element_line(color = "gray60", linetype = "dashed", linewidth = .5),
    ) +  geom_rect(aes(xmin = box_xmin, xmax = box_xmax, ymin = box_ymin, ymax = box_ymax),
                   fill = "gray94", color = "gray94", linetype = "solid", linewidth = 1.2) +
    annotate("text", x = 5, y = 20, label = paste0("μ = ", round(stats$expected, 3), "\n", 
                                                   "3σ = ", round(3*sigma, 3), "\n", 
                                                   "Δσ = ", round(delta, 3), "\n", 
                                                   "P(Draw) = ", round(stats$prob_draw, 3), "%"),
             color = "black", hjust = 0 ,vjust = 1)

}

plot_histogram(probability_df, legend_data, team_colors, stats, team_name, delta_territories)