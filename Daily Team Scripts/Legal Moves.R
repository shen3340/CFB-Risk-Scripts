library(httr)
library(jsonlite)
library(stringr)
season <- 5
day <- 25
myteam <- "March"
excluded_ids <- c(249, 186)  # Bermuda/Sicily untouchable territories

# Fetch and parse data
url <- sprintf("https://collegefootballrisk.com/api/territories?day=%d&season=%d", day, season)
data <- fromJSON(content(GET(url), "text", encoding = "UTF-8"))
  data <- transform(data, neighbors = lapply(as.character(neighbors), function(n) {
    as.integer(unlist(str_extract_all(n, "\\b\\d+\\b")))
  }))


# Identify territories
defend_territories <- with(data, name[owner == myteam & 
                                        sapply(neighbors, \(n) any(owner[id %in% n] != myteam))])
attack_territories <- with(data, name[
  id %in% unique(unlist(neighbors[owner == myteam])) & 
    owner != myteam & 
    !id %in% excluded_ids
])

# Output to file
writeLines(c(
  "DEFEND:", sort(defend_territories), "",
  "ATTACK:", sort(attack_territories)
), paste0("Daily Team Scripts/Legal_Moves_Day_", day, ".txt"))
