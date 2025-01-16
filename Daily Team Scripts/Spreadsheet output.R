library(httr)
library(jsonlite)
library(tidyverse)
library(openxlsx)
myteam <- "March"
output_file <- paste0("Daily Team Scripts/", myteam, "_player_stats.xlsx")
team_url <- paste0("https://collegefootballrisk.com/api/players?team=", myteam)

# Fetch and process data
response <- GET(team_url)

if (status_code(response) == 200) {
  parsed_team_data <- fromJSON(content(response, as = "text", encoding = "UTF-8"))
  
  get_player_data <- function(player_name) {
    player_url <- paste0("https://collegefootballrisk.com/api/player?player=", player_name)
    player_response <- GET(player_url)
    if (status_code(player_response) == 200) {
      parsed <- fromJSON(content(player_response, as = "text", encoding = "UTF-8"))
      tibble(
        Name = sub("\\$0$", "", player_name),
        Stars = parsed$ratings$overall,
        Total_turns = parsed$stats$totalTurns,
        Round_turns = parsed$stats$gameTurns,
        Streak = parsed$stats$streak,
        MVPs = parsed$stats$mvps
      )
    } else {
      NULL
    }
  }
  
  # Fetch player stats and arrange
  player_stats <- parsed_team_data$player %>%
    map_df(get_player_data) %>%
    arrange(desc(Stars), Name)
  
  # Define styles for stars
  styles <- setNames(
    map(c("darkgreen", "lightgreen", "yellow", "orange", "red"), ~ createStyle(fgFill = ., fontColour = ifelse(. == "darkgreen", "white", "black"))),
    c(5, 4, 3, 2, 1)
  )
  
  # Write to workbook with styles
  wb <- createWorkbook()
  addWorksheet(wb, "Player Stats")
  writeData(wb, "Player Stats", player_stats)
  
  walk2(player_stats$Stars, seq_len(nrow(player_stats)) + 1, ~ {
    if (.x %in% names(styles)) {
      addStyle(wb, "Player Stats", styles[[as.character(.x)]], rows = .y, cols = 1:ncol(player_stats), gridExpand = TRUE)
    }
  })
  
  saveWorkbook(wb, output_file, overwrite = TRUE)
  message("Excel file with conditional formatting saved as ", output_file)
} else {
  message("Error fetching team data: ", status_code(response))
}
