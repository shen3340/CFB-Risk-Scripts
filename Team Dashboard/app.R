library(shiny)
library(httr)
library(jsonlite)
library(tidyverse)
library(gt)
library(gtExtras)
# API base URL
base <- "https://dough.collegefootballrisk.com/api"
setwd("C:/Users/shen3/OneDrive/Desktop/R/Risk/CFB-Risk-Scripts/Team Dashboard")

# Fetch teams data from the API
fetch_teams_data <- function() {
  response <- GET("https://dough.collegefootballrisk.com/api/teams")
  if (status_code(response) == 200) {
    teams_data <- fromJSON(content(response, "text", encoding = "UTF-8"))
    return(teams_data)
  } else {
    stop("Failed to fetch teams data.")
  }
}

# Prepare the teams data
teams_data <- fetch_teams_data()

# Extract unique seasons
all_seasons <- sort(unique(unlist(teams_data$seasons)))

# Username Mapping and Exclusions
excluded_ids <- c(249, 186)  # Bermuda/Sicily untouchable territories
reddit_usernames <- c("AbundantFailure", "bb06ta", "HonestDig1", "madcel56", 
                      "randomguy84321", "SnooDogs375", "TopStuff513", "truetoatlanta17", "Vast_Field2374")

username_mapping <- c("Shaller13" = "shaller88", "32RH" = "32rh", 
                      "bobsappisfat" = "bobsappisfat1", "Janus67" = "janus67", "Mavyn1" = "mavyn1",
                      "BrBuckeye1" = "brbuckeye1", "MochasAway" = "mochasaway", "rax96" = "rax96max",  
                      "doom_bagel" = "uncle_bagel", "Forwhom" = "jgabby", "Mautamu" = "mautam", 
                      "slappinDingers1" = "slappindingers1","GoBucks513" = "thetmviking11b", 
                      "narcolepszzz" = "twoduckchuck","EpicWolverine" = "epicwolverine")

clean_username <- function(usernames) {
  sapply(usernames, function(u) {
    # Map username if it exists in username_mapping, otherwise use original
    mapped_username <- ifelse(u %in% names(username_mapping), username_mapping[u], u)
    # Remove trailing "$0" from the mapped or original username
    gsub("\\$0$", "", mapped_username)
  }, USE.NAMES = FALSE)
}

# Helper Function: Fetch Team Odds
get_odds <- function(team, season, day) {
  team_odds <- GET(paste0(base, "/team/odds"), query = list(team = team, season = season, day = day)) %>%
    content("text", encoding = "UTF-8") %>%
    fromJSON() %>%
    select(territory, winner, chance) %>%
    arrange(desc(chance), territory)
  
  # Fetch additional territory data
  territory_data <- team_odds$territory %>%
    unique() %>%
    map_df(~{
      GET("https://dough.collegefootballrisk.com/api/territory/turn", query = list(territory = .x, season = season, day = day)) %>%
        content("text", encoding = "UTF-8") %>%
        fromJSON() %>%
        .$teams %>%
        as.data.frame() %>%
        select(team, players, power) %>%
        mutate(territory = .x)
    }) %>%
    bind_rows() %>%
    group_by(territory) %>%
    arrange(desc(team == team), .by_group = TRUE) |> 
    summarize(
      combined_info = ifelse(any(players > 0), paste(
        team, ":", players, " Players, ", round(power, 2), " Power", collapse = "|"
      ), "")
    )
  
  team_odds %>%
    left_join(territory_data, by = "territory")
}

# Helper Function: Get Legal Moves
get_legal_moves <- function(season, day, myteam) {
  url <- sprintf("https://dough.collegefootballrisk.com/api/territories?day=%s&season=%s", day, season)
  data <- fromJSON(content(GET(url), "text", encoding = "UTF-8"))
  
  data <- transform(data, neighbors = lapply(as.character(neighbors), function(n) {
    as.integer(unlist(str_extract_all(n, "\\b\\d+\\b")))
  }))
  
  defend_territories <- with(data, name[owner == myteam & 
                                          sapply(neighbors, \(n) any(owner[id %in% n] != myteam))])
  
  attack_territories <- with(data, name[
    id %in% unique(unlist(neighbors[owner == myteam])) & 
      owner != myteam & 
      !id %in% excluded_ids
  ])
  
  defend_territories <- sort(defend_territories)
  attack_territories <- sort(attack_territories)
  
  return(list(defend = defend_territories, attack = attack_territories))
}

# Helper Function: Fetch MVP Players
get_mvp_players <- function(team, season, day) {
  url <- paste0(base, "/team/players?season=", season, "&day=", day, "&team=", team)
  response <- GET(url)
  if (status_code(response) == 200) {
    data <- fromJSON(content(response, "text", encoding = "UTF-8"))
    if ("mvp" %in% names(data) && is.logical(data$mvp)) {
      mvp_players <- data$player[data$mvp]
      return(clean_username(mvp_players))  # Apply the clean_username function
    }
  }
  return(character())
}



# UI
ui <- fluidPage(
  titlePanel("College Football Risk Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("season", "Select Season:", choices = all_seasons),
      uiOutput("team_ui"),
      numericInput("day", "Enter Day:", value = 1, min = 1),
      actionButton("fetch", "Get Data")
    ),
    
    mainPanel(
      h3("SVG Map:"),
      htmlOutput("map"),
      
      h3("Team Odds:"),
      tableOutput("oddsTable"),
      
      h3("MVP Players:"),
      verbatimTextOutput("mvpList"),
      
      h3("Legal Moves:"),
      verbatimTextOutput("legalMoves")
    )
  )
)

# Server
server <- function(input, output, session) {
  
  # Update teams based on selected season
  observeEvent(input$season, {
    selected_season <- as.numeric(input$season)
    available_teams <- teams_data %>%
      filter(sapply(seasons, function(season_list) selected_season %in% season_list)) %>%
      pull(name) %>%
      sort()
    
    updateSelectInput(session, "team", choices = available_teams)
  })
  
  
  # Render team selection UI
  output$team_ui <- renderUI({
    selectInput("team", "Select Team:", choices = NULL)
  })
  
  output$map <- renderUI({
    tags$div(
      HTML(readLines("www/map.svg", warn = FALSE))  # Read SVG as HTML
    )
  })
  
  
  # Fetch and display data when 'Fetch Data' is clicked
  observeEvent(input$fetch, {
    req(input$team, input$season, input$day)
    
    # Fetch Odds
    odds_data <- get_odds(input$team, input$season, input$day)
    
    # Display Odds Table
    output$oddsTable <- renderTable({
      odds_data %>%
        mutate(chance = sprintf("%.2f%%", chance * 100)) %>%
        select(Territory = territory, Winner = winner, "Odds" = chance, "Territory Data" = combined_info)
    })
    
    # Fetch MVP Players
    mvp_players <- get_mvp_players(input$team, input$season, input$day)
    
    output$mvpList <- renderPrint({
      if (length(mvp_players) == 0) {
        "No MVPs found."
      } else {
        paste(paste(mvp_players, collapse = ", "))
      }
    })
    
    # Fetch Legal Moves
    legal_moves <- get_legal_moves(input$season, input$day + 1, input$team)
    
    output$legalMoves <- renderPrint({
      paste("DEFEND:", paste(legal_moves$defend, collapse = ", "), "\n",
            "ATTACK:", paste(legal_moves$attack, collapse = ", "))
    })
  })
}


# Run the App
shinyApp(ui, server)
