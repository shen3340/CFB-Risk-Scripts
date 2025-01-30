library(httr)
library(tidyverse)
library(ggthemes)
library(showtext)
base <- "https://collegefootballrisk.com/api"
season <- 5
day <- 25

# Helper functions 
fetch_api_data <- function(endpoint, query = list()) {
  response <- GET(paste0(base, endpoint), query = query)
  content(response, "parsed")
}
get_team_colors <- function(team_name) fetch_api_data("/teams") %>% 
  keep(~ .x$name == team_name) %>% first() %>% .[["colors"]] %>% 
  .[c("primary", "secondary")]
get_team_data <- function(team_name, day, season) {
  fetch_api_data("/team/odds", list(season = season, day = day, team = team_name))
}
get_leaderboard_data <- function(season, day) 
  fetch_api_data("/stats/leaderboard", list(season = season, day = day))

# Get leaderboard data
leaderboard_data <- get_leaderboard_data(season, day)

# Function to calculate statistics for a team
calculate_statistics <- function(team_data, team_name) {
  odds <- map_dbl(team_data, ~ ifelse(.x$territoryPower > 0, .x$teamPower / .x$territoryPower, 1))
  actual <- sum(map_lgl(team_data, ~ .x$winner == team_name))
  expected <- sum(odds)
  
  vals <- reduce(odds, ~ convolve(.x, rev(c(1 - .y, .y)), type = "open"), .init = 1)
  vals <- vals / sum(vals)  # Normalize
  prob_draw <- ifelse(actual + 1 <= length(vals), 100 * vals[actual + 1], 0)
  list(actual = actual, expected = expected, vals = vals, prob_draw = prob_draw)
}

# Function to generate the plot for a team
plot_histogram <- function(probability_df, legend_data, colors, stats, team_name, delta_territories) {
  suppressWarnings(
    ggplot(probability_df, aes(x = Territories, y = Probability)) +
      geom_bar(stat = "identity", fill = colors$primary, color = colors$secondary, position = position_identity()) +
      geom_vline(data = legend_data, aes(xintercept = xintercept, 
                                         color = factor(label, levels = legend_levels), 
                                         linetype = factor(label, levels = legend_levels), alpha = alpha), 
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
        subtitle = paste("<i>Expected:</i>", round(stats$expected, 2), ", <i>Actual:</i>", stats$actual, ", <i>ΔTerritories=</i>", delta_territories),
        x = "Number of Territories Won",
        y = "Percent Chance to Win N Territories (%)"
      ) +
      theme_hc() +
      theme(
        plot.margin = margin(1, 1, 1, 1, "cm"),
        panel.background = element_rect(fill = "gray92", color = NA),
        legend.background = element_rect(fill = "gray94", color = NA),
        plot.subtitle = ggtext::element_markdown(family = "serif", size = 20, hjust = 0.5),
        plot.title = element_text(size = 22, hjust = 0.5),
        legend.text = element_text(size = 15),
        legend.position = c(legend_x, .95),  # Moves legend to the top-center
        legend.justification = c(0.5, 1),  # Aligns legend properly at the top
        legend.direction = "vertical", 
        legend.title = element_blank(),
        axis.title = element_text(size = 20),
        axis.text = element_text(size = 18, colour = "black"),
        panel.grid.major = element_line(color = "gray60", linewidth = 0.5, linetype = "dashed"),  # Major gridlines
        panel.grid.major.x = element_line(linetype = "dashed"),
        panel.grid.major.y = element_line(color = "gray60", linetype = "dashed", linewidth = .5),
      ) + 
      annotate("label", x = min(probability_df$Territories) + increment,  
               y = min(probability_df$Probability) + 2 * y_increment,  
               label = paste0("μ = ", round(stats$expected, 3), "\n", 
                              "3σ = ", round(3*sigma, 3), "\n", 
                              "Δσ = ", round(delta, 3), "\n", 
                              "P(Draw) = ", round(stats$prob_draw, 3), "%"),
               fill = scales::alpha("gray94", 0.5), color = "black", size = 6, 
               hjust = 0, vjust = 1, label.size = 0)
  )
}

# Iterate over each team in the leaderboard and generate the plot
for (i in seq_along(leaderboard_data)) {
  team_name <- leaderboard_data[[i]]$name
  team_colors <- get_team_colors(team_name)
  team_data <- get_team_data(team_name, day, season)
  territories_day_prev <- get_leaderboard_data(season, day - 1) %>%
    keep(~ .x$name == team_name) %>% first() %>% .[["territoryCount"]]
  
  # Calculate statistics
  stats <- calculate_statistics(team_data, team_name)
  
  # Prepare data for plotting
  probability_df <- tibble(
    Territories = seq_along(stats$vals) - 1,
    Probability = stats$vals * 100
  )
  sigma <- sqrt(sum(probability_df$Probability * (probability_df$Territories - stats$expected)^2) / sum(probability_df$Probability))
  delta_territories <- stats$actual - territories_day_prev
  territories_oe <- stats$actual - stats$expected
  delta <- ifelse(sigma == 0, 0, (stats$actual - stats$expected) / sigma)
  
  # Normal distribution curve
  normal_curve <- tibble(Territories = seq(min(probability_df$Territories), max(probability_df$Territories), length.out = 100),
                         Probability = dnorm(Territories, mean = stats$expected, sd = sigma) / max(dnorm(Territories, mean = stats$expected, sd = sigma)) * max(probability_df$Probability))
  
  # Prepare legend data
  legend_levels <- c("X ~ N(μ,σ)", "Expected Value","Actual Territories", "Prev Num. Territories")
  legend_data <- tibble(
    xintercept = c(0, stats$expected , stats$actual, territories_day_prev),
    label = c("X ~ N(μ,σ)", "Expected Value", "Actual Territories",  "Prev Num. Territories"),
    color = c("gray34", "#081840" , ifelse(territories_oe < 0, "#781b0e", "#3b8750"),  "#ffb521"),
    linetype = "dashed", alpha = ifelse(label == "X ~ N(μ,σ)", 0, 1))
  
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
    legend_x <- 0.25  # Default to top-left
  }
  
  # Generate and save the plot for each team
  output_dir <- paste0("Daily Summary Scripts/", day)
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)
  }
  path <- paste0(output_dir, "/", i, "_", team_name, ".png")
  
  # Save the plot with 150 dpi
  ggsave(filename = path, plot = plot_histogram(probability_df, legend_data, team_colors, stats, team_name, delta_territories), dpi = 150, width = 10, height = 8)
}
