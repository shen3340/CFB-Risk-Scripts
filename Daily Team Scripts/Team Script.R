library(httr)
library(gt)
library(gtExtras)
library(jsonlite)
library(stringr)
library(tidyverse)
base <- "https://collegefootballrisk.com/api"
day <- 27
season <- 5
myteam <- "March"
excluded_ids <- c(249, 186)  # Bermuda/Sicily untouchable territories
folder <- folder <- paste0("Daily Team Scripts/", season, "/", day)
mvp_url <- paste0(base, "/team/players?season=", season, "&day=", day, "&team=", myteam)

odds_path <- paste0(folder, "/Odds.png")
reddit_usernames <- c("AbundantFailure", "bb06ta", "HonestDig1", "madcel56", 
                      "randomguy84321", "SnooDogs375", "TopStuff513", "truetoatlanta17", "Vast_Field2374")

username_mapping <- c("Shaller13" = "shaller88", "32RH" = "32rh", 
                      "bobsappisfat" = "bobsappisfat1", "Janus67" = "janus67", "Mavyn1" = "mavyn1",
                      "BrBuckeye1" = "brbuckeye1", "MochasAway" = "mochasaway", "rax96" = "rax96max",  
                      "doom_bagel" = "uncle_bagel", "Forwhom" = "jgabby", "Mautamu" = "mautam", 
                      "slappinDingers1" = "slappindingers1","GoBucks513" = "thetmviking11b", 
                      "narcolepszzz" = "twoduckchuck","EpicWolverine" = "epicwolverine")
if (!dir.exists(folder)) {
  dir.create(folder, recursive = TRUE)
}

# Helper Functions

clean_username <- function(usernames) {
  sapply(usernames, function(u) {
    # Map username if it exists in username_mapping, otherwise use original
    mapped_username <- ifelse(u %in% names(username_mapping), username_mapping[u], u)
    # Remove trailing "$0" from the mapped or original username
    gsub("\\$0$", "", mapped_username)
  }, USE.NAMES = FALSE)
}

get_odds <- function(team, season, day) {
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

get_legal_moves <- function(season, day, myteam, excluded_ids = c()) {
  # Fetch and parse data
  url <- sprintf("https://collegefootballrisk.com/api/territories?day=%d&season=%d", day, season)
  data <- fromJSON(content(GET(url), "text", encoding = "UTF-8"))
  
  # Convert neighbor strings to integer lists
  data <- transform(data, neighbors = lapply(as.character(neighbors), function(n) {
    as.integer(unlist(str_extract_all(n, "\\b\\d+\\b")))
  }))
  
  # Identify territories to defend
  defend_territories <- with(data, name[owner == myteam & 
                                          sapply(neighbors, \(n) any(owner[id %in% n] != myteam))])
  
  # Identify territories to attack
  attack_territories <- with(data, name[
    id %in% unique(unlist(neighbors[owner == myteam])) & 
      owner != myteam & 
      !id %in% excluded_ids
  ])
  
  # Define output file name
  output_file <- sprintf("Daily Team Scripts/%d/%d/Legal_Moves.txt", season, day - 1)
  # Write to file
  writeLines(c(
    "DEFEND:", sort(defend_territories), "",
    "ATTACK:", sort(attack_territories)
  ), output_file)
  
  message(sprintf("Legal moves saved to %s", output_file))
}

get_mvp_players <- function(url) {
  response <- GET(url)
  if (status_code(response) == 200) {
    data <- fromJSON(content(response, "text", encoding = "UTF-8"))
    return(if ("mvp" %in% names(data) && is.logical(data$mvp)) data$player[data$mvp] else character())
  }
  warning("Failed to fetch MVP data.")
  character()
}

## Main Workflow

mvp_players <- get_mvp_players(mvp_url)

set_discord_players <- setdiff(mvp_players, reddit_usernames) %>% clean_username() %>% sort()
set_reddit <- intersect(mvp_players, reddit_usernames) %>% setdiff("madcel56") %>% sort()

write_total_mvp <- paste(
  "Congratulations to the following Marchers who won MVP for tonight's roll:",
  paste0("@", c(set_discord_players, set_reddit), collapse = " ")
)

writeLines(write_total_mvp, sprintf("Daily Team Scripts/%d/%d/MVP.txt", season, day))

get_odds(myteam, season, day) %>%
  gt() %>%
  fmt_percent(columns = chance, decimals = 2) %>%
  cols_label(
    territory = "Territory", winner = "Winner", chance = "Odds", combined_info = "Territory Data"
  ) %>%
  opt_row_striping() %>%
  gt_theme_guardian() %>%
  fmt_markdown(columns = combined_info) %>%
  tab_style(style = cell_text(align = "center"), locations = cells_column_labels(columns = everything())) %>%
  gtsave(odds_path, expand = 10)

get_legal_moves(season, day + 1, myteam, excluded_ids)