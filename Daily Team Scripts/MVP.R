library(httr)
library(jsonlite)
library(tidyverse)
season <- 5
day <- 5
myteam <- "March"

# Mapping for Discord usernames
username_mapping <- c("Shaller13" = "shaller88", "32RH" = "32rh", 
                      "bobsappisfat" = "bobsappisfat1", "Janus67" = "janus67", "Mavyn1" = "mavyn1",
                      "BrBuckeye1" = "brbuckeye1", "MochasAway" = "mochasaway", "rax96" = "rax96max",  
                      "doom_bagel" = "uncle_bagel", "Forwhom" = "jgabby", "Mautamu" = "mautam", 
                      "slappinDingers1" = "slappindingers1","GoBucks513" = "thetmviking11b", 
                      "narcolepszzz" = "twoduckchuck","EpicWolverine" = "epicwolverine")

# Fetch MVP player names
get_mvp_players <- function(url) {
  response <- GET(url)
  if (status_code(response) == 200) {
    data <- fromJSON(content(response, "text", encoding = "UTF-8"))
    return(if ("mvp" %in% names(data) && is.logical(data$mvp)) data$player[data$mvp] else character())
  }
  warning("Failed to fetch MVP data.")
  character()
}

clean_username <- function(usernames) {
  sapply(usernames, function(u) {
    # Map username if it exists in username_mapping, otherwise use original
    mapped_username <- ifelse(u %in% names(username_mapping), username_mapping[u], u)
    # Remove trailing "$0" from the mapped or original username
    gsub("\\$0$", "", mapped_username)
  }, USE.NAMES = FALSE)
}

# Main logic
mvp_url <- paste0("https://collegefootballrisk.com/api/team/players?season=", season, "&day=", day, "&team=", myteam)
mvp_players <- get_mvp_players(mvp_url)
reddit_usernames <- c("AbundantFailure", "bb06ta", "HonestDig1", "madcel56", 
                      "randomguy84321", "SnooDogs375", "TopStuff513", "truetoatlanta17", "Vast_Field2374")

discord_players <- setdiff(mvp_players, reddit_usernames) %>% clean_username() %>% sort()
mvp_reddit <- intersect(mvp_players, reddit_usernames) %>% setdiff("madcel56") %>% sort()

final_result <- paste(
  "Congratulations to the following Marchers who won MVP for tonight's roll:",
  paste0("@", c(discord_players, mvp_reddit), collapse = " ")
)

writeLines(final_result, paste0("Daily Team Scripts/MVP ", season, "-", day, ".txt"))