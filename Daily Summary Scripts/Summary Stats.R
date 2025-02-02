# Setting libraries, paths, and variables
library(gt)
library(ggthemes)
library(httr)
library(jsonlite)
library(knitr)
library(purrr)
library(showtext)
library(tidyverse)
library(Unicode)
base <- "https://collegefootballrisk.com/api"
chaos <- "Undecimber"
day <- 27
folder <- paste0("Daily Summary Scripts/Day ", day)
italic_a <- u_char_inspect(u_char_from_name("MATHEMATICAL ITALIC SMALL A"))["Char"]
italic_D <- u_char_inspect(u_char_from_name("MATHEMATICAL ITALIC CAPITAL D"))["Char"]
italic_mu <- u_char_inspect(u_char_from_name("MATHEMATICAL ITALIC SMALL MU"))["Char"]
italic_sigma <- u_char_inspect(u_char_from_name("MATHEMATICAL ITALIC SMALL SIGMA"))["Char"]
italic_r <- u_char_inspect(u_char_from_name("MATHEMATICAL ITALIC SMALL R"))["Char"]
italic_w <- u_char_inspect(u_char_from_name("MATHEMATICAL ITALIC SMALL W"))["Char"]
milestones <- c(50, 100, 150, 200, 250)
season <- 5
territory_path <- paste0(folder, "/Luckiest Territories.png")
if (!dir.exists(folder)) {
  dir.create(folder, recursive = TRUE)
}


# Helper functions

calculate_statistics <- function(team_data, team_name) {
  odds <- map_dbl(team_data, ~ ifelse(.x$territoryPower > 0, .x$teamPower / .x$territoryPower, 1))
  actual <- sum(map_lgl(team_data, ~ .x$winner == team_name))
  expected <- sum(odds)
  
  vals <- reduce(odds, ~ convolve(.x, rev(c(1 - .y, .y)), type = "open"), .init = 1)
  vals <- vals / sum(vals)  # Normalize
  prob_draw <- ifelse(actual + 1 <= length(vals), 100 * vals[actual + 1], 0)
  list(actual = actual, expected = expected, vals = vals, prob_draw = prob_draw)
}

convert_rgba_to_hex <- function(rgba) {
  # Extract numbers from "rgba(r,g,b,a)" format
  numbers <- as.numeric(str_extract_all(rgba, "[0-9]+")[[1]])
  if (length(numbers) >= 3) {
    rgb(numbers[1], numbers[2], numbers[3], maxColorValue = 255)
  } else {
    "#000000"  # Default to black if conversion fails
  }
}

fetch_all_odds <- function(teams, season, day) {
  map_dfr(teams$name, ~fetch_team_odds(.x, season, day))
}

fetch_api_data <- function(endpoint, query = list()) {
  response <- GET(paste0(base, endpoint), query = query)
  content(response, "parsed")
}

fetch_data <- function(url, query = list()) {
  response <- GET(url, query = query)
  stop_for_status(response)
  content(response, "text", encoding = "UTF-8") %>%
    fromJSON(flatten = TRUE) %>%
    as_tibble()
}

fetch_players <- function(teams, season, day, milestones) {
  teams$name %>%
    map_dfr(function(team) {
      players_url <- paste0(base, "/players?team=", team)
      players_data <- safely(fetch_data)(players_url)$result
      if (!is.null(players_data) && "turnsPlayed" %in% names(players_data)) {
        players_data %>%
          filter(season == season, day == day, turnsPlayed %in% milestones)
      } else {
        tibble()
      }
    })
}

fetch_teams <- function(season) {
  url <- paste0(base, "/teams?season=", season)
  fetch_data(url) %>%
    mutate(seasons = map_dbl(seasons, ~ max(unlist(.x)))) %>%
    filter(seasons == season)
}

fetch_team_odds <- function(team, season, day) {
  # Try fetching data and handle errors
  response <- safely(GET)(paste0(base, "/team/odds?team=", team, "&season=", season, "&day=", day))
  
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

get_leaderboard_data <- function(season, day) {
  fetch_api_data("/stats/leaderboard", list(season = season, day = day))}

get_luckiest_territories <- function(data, top_n = 5) {
  data %>%
    filter(team == winner, chance < 0.5) %>%
    group_by(territory) %>%
    mutate(min_chance = min(chance)) %>%
    filter(chance == min_chance) %>%
    ungroup() %>%
    arrange(chance) %>%
    head(top_n)
}

get_team_colors <- function(team_name) {
  team_data <- fetch_api_data("/teams") %>% 
    keep(~ .x$name == team_name) %>% 
    first()
  
  if (is.null(team_data)) {
    stop("Team not found in API data")
  }
  
  colors <- team_data[["colors"]]
  
  list(
    primary = ifelse(startsWith(colors$primary, "#"), colors$primary, convert_rgba_to_hex(colors$primary)),
    secondary = ifelse(startsWith(colors$secondary, "#"), colors$secondary, convert_rgba_to_hex(colors$secondary))
  )
}

get_team_data <- function(team_name, day, season) {
  fetch_api_data("/team/odds", list(season = season, day = day, team = team_name))
}

is_valid_odds <- function(odds) {
  !is.null(odds) &&
    is.data.frame(odds) &&
    all(c("territory", "winner", "chance") %in% colnames(odds))
}

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
      scale_color_manual(values = setNames(legend_data$color, legend_levels), 
                         labels = legend_levels)  # Ensure labels use expressions
    + scale_linetype_manual(values = setNames(legend_data$linetype, legend_levels), 
                            labels = legend_levels) + 
      scale_x_continuous(
        breaks = seq(min(probability_df$Territories), max(probability_df$Territories), by = increment),
        limits = c(0, ifelse(stats$actual == max(probability_df$Territories), max(probability_df$Territories) + increment, max(probability_df$Territories))),  
        expand = c(0, 0),
        labels = function(x) ifelse(x == 0 & !exists("zero_shown", envir = .GlobalEnv), { assign("zero_shown", TRUE, envir = .GlobalEnv); "0" }, ifelse(x == 0, "", as.character(x)))
      ) +
      scale_y_continuous(
        breaks = seq(0, y_max, by = y_increment),  # Ensure breaks include the rounded max
        limits = c(0, y_max),  # Extend the upper limit
        expand = c(0, 0),
        labels = function(y) ifelse(y == 0 & !exists("zero_shown", envir = .GlobalEnv), { assign("zero_shown", TRUE, envir = .GlobalEnv); "0" }, ifelse(y == 0, "", as.character(y)))
      ) +  
      labs(
        title = paste("Number of Territories Histogram:", team_name),
        subtitle = paste("<i>Expected:</i>", round(stats$expected, 2), ", <i>Actual:</i>", stats$actual, ", <i>ΔTerritories=</i>", delta_territories),
        x = "Number of Territories Won",
        y = "Percent Chance to Win N Territories (%)"
      ) +
      theme_hc() +
      theme(
        plot.margin = margin(.5, 1, .5, 1, "cm"),
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
      annotate("label", x = min(probability_df$Territories) + increment * 0.5,  
               y = min(probability_df$Probability) + 3 * y_increment,  
               label = paste0(label1, "\n", 
                              label2,  "\n", 
                              label3,  "\n", 
                              label4),
               fill = scales::alpha("gray94", 0.5), color = "black", size = 6, 
               hjust = 0, vjust = 1, label.size = 0)
  )
}

# Main workflow

leaderboard_data <- get_leaderboard_data(season, day)
teams_data <- fetch_teams(season)
teams_odds <- fetch_all_odds(teams_data, season, day)
team_stats_list <- list()

territory_luck <- get_luckiest_territories(teams_odds, top_n = 5)
territory_num <- nrow(territory_luck)

# Generate the table
territory_luck %>%
  select(territory, winner, chance) %>%
  gt() %>%
  fmt_percent(columns = chance, decimals = 2) %>%
  cols_label(
    territory = "Territory",
    winner = "Winner",
    chance = "Winning Odds (%)"
  ) %>%
  tab_header(
    title = paste0("Top ", territory_num, " Luckiest Territories"),
    subtitle = paste0("Season ", season, ", Day ", day)
  ) |> 
  gtsave(territory_path, expand = 10)

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
  suppressWarnings(sigma <- sqrt(sum(probability_df$Probability * (probability_df$Territories - stats$expected)^2) / sum(probability_df$Probability)))
  delta_territories <- stats$actual - territories_day_prev
  territories_oe <- stats$actual - stats$expected
  delta <- ifelse(sigma == 0, 0, (stats$actual - stats$expected) / sigma)
  
  team_stats_list[[i]] <- tibble(
    Team = team_name,
    Delta = round(delta, 3)
  )
  
  # Normal distribution curve
  normal_curve <- tibble(Territories = seq(min(probability_df$Territories), max(probability_df$Territories), length.out = 100),
                         Probability = dnorm(Territories, mean = stats$expected, sd = sigma) / max(dnorm(Territories, mean = stats$expected, sd = sigma)) * max(probability_df$Probability))
  
  # Prepare legend data
  legend_levels <- c(expression(italic("X ~ N(μ,σ)")), "Expected Value", 
                     "Actual Territories", 
                     "Prev Num. Territories")
  
  legend_data <- tibble(
    xintercept = c(0, stats$expected , stats$actual, territories_day_prev),
    label = legend_levels,
    color = c("gray34", "#081840" , ifelse(territories_oe < 0, "#781b0e", "#3b8750"),  "#ffb521"),
    linetype = "dashed", alpha = ifelse(label == "X ~ N(μ,σ)", 0, 1))
  
  label1 <- paste(italic_mu, " = ", round(stats$expected, 3), sep="")
  label2 <- paste("3", italic_sigma, " = ", round(3*sigma, 3), sep = "")
  label3 <- paste("Δ", italic_sigma, " = ", round(delta, 3), sep = "")
  label4 <- paste("P(", italic_D, italic_r, italic_a, italic_w, ") = ", round(stats$prob_draw, 3), "%", sep = "")
  #"P(Draw) = ", round(stats$prob_draw, 3), "%"),
  
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
  
  hist_path <- paste0(folder, "/", i, "_", team_name, ".png")
  
  # Save the plot with 150 dpi
  suppressWarnings({
    ggsave(filename = hist_path, 
           plot = plot_histogram(probability_df, legend_data, team_colors, stats, team_name, delta_territories), 
           dpi = 150, 
           width = 10, 
           height = 8)
  })
}

team_stats_df <- bind_rows(team_stats_list)
luckiest_teams <- team_stats_df %>% arrange(desc(Delta)) %>% head(3) |> 
  rename("Luckiest Teams" = Team, "Today's Sigma" = Delta)
unluckiest_teams <- team_stats_df %>% arrange(Delta) %>% head(3) |> 
  rename("Unluckiest Teams" = Team, "Today's Sigma" = Delta)
print(kable(luckiest_teams, format = "simple"))
print(kable(unluckiest_teams, format = "simple"))

players_meeting_criteria <- fetch_players(teams_data, season, day, milestones)
generate_milestone_messages(players_meeting_criteria, milestones)