
library(httr)
library(jsonlite)
library(tidyverse)
library(gtExtras)
library(gt)
season <- 5
day <- 4
myteam <- "March"
filename <- paste0("Odds Season ", season, " day ", day, ".png")

# Fetch team odds and territory data in one go
fetch_data <- function(team, season, day) {
  team_odds <- GET("https://collegefootballrisk.com/api/team/odds", query = list(team = team, season = season, day = day)) %>%
    content("text", encoding = "UTF-8") %>%
    fromJSON() %>%
    select(territory, winner, chance) %>%
    arrange(desc(chance), territory)
  
  territory_data <- team_odds$territory %>%
    unique() %>%
    map_df(~{
      GET("https://collegefootballrisk.com/api/territory/turn", query = list(territory = .x, season = season, day = day)) %>%
        content("text", encoding = "UTF-8") %>%
        fromJSON() %>%
        .$teams %>%
        as.data.frame() %>%
        select(team, players, power) %>%
        mutate(territory = .x)
    }) %>%
    bind_rows() %>%
    group_by(territory) %>%
    arrange(desc(team == myteam), .by_group = TRUE) |> 
    summarize(
      combined_info = ifelse(any(players > 0), paste(
        team, ":", players, " Players, ", round(power, 2), " Power", collapse = "|"
      )
      , "")
    )
  
  team_odds %>%
    left_join(territory_data, by = "territory")
}

# Generate and save table
fetch_data(myteam, season, day) %>%
  gt() %>%
  fmt_percent(columns = chance, decimals = 2) %>%
  cols_label(
    territory = "Territory", winner = "Winner", chance = "Odds", combined_info = "Territory Data"
  ) %>%
  opt_row_striping() %>%
  gt_theme_guardian() %>%
  fmt_markdown(columns = combined_info) %>%
  tab_style(style = cell_text(align = "center"), locations = cells_column_labels(columns = everything())) %>%
  gtsave(filename, expand = 10)
