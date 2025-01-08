library(ggplot2)
library(ggtext)
library(glue)
library(httr)
library(jsonlite)

mean <- 20
sd <- 5
delta <- 2
expected <- 22
actual <- 25
prev_num_terry <- 18  # Previous number of territories
myteam <- "October"
actual_color <- "red"

# Function to convert RGBA to Hex
rgba_to_hex <- function(rgba) {
  # Extract the RGBA components
  rgba_values <- as.numeric(unlist(strsplit(gsub("rgba\\(|\\)", "", rgba), ",")))
  
  # Normalize RGB to 0-255 range and convert to Hex
  rgb_hex <- rgb(rgba_values[1], rgba_values[2], rgba_values[3], maxColorValue = 255)
  
  return(rgb_hex)
}

# Define the function to get team colors
get_team_colors <- function(team_name) {
  # Define the API URL
  api_url <- "https://collegefootballrisk.com/api/teams"
  
  # Fetch data from the API
  response <- GET(api_url)
  
  # Check if the request was successful
  if (status_code(response) == 200) {
    # Parse the JSON response
    team_data <- fromJSON(content(response, "text", encoding = "UTF-8"))
    
    # Find the team entry based on the team name
    team_info <- team_data[sapply(team_data$name, function(x) x == team_name), ]
    
    if (nrow(team_info) > 0) {
      # Extract primary and secondary colors
      primary_color <- team_info$colors$primary
      secondary_color <- team_info$colors$secondary
      
      # Check if the color is in RGBA format and convert to Hex if needed
      if (grepl("rgba", primary_color)) {
        primary_color <- rgba_to_hex(primary_color)
      }
      if (grepl("rgba", secondary_color)) {
        secondary_color <- rgba_to_hex(secondary_color)
      }
      
      # Return the colors as a list in Hex format
      return(list(primary = primary_color, secondary = secondary_color))
    } else {
      # Return a message if the team is not found
      return(paste("Team", team_name, "not found."))
    }
  } else {
    # Return an error message if the API request fails
    return(paste("API request failed with status:", status_code(response)))
  }
}

# Example usage
team_colors <- get_team_colors(myteam)

primary_color <- team_colors$primary
secondary_color <- team_colors$secondary

#### Simulate data ----
data <- data.frame(values = rnorm(1000, mean = mean, sd = sd))

# Data frame for vertical lines
vline_data <- data.frame(
  xintercept = c(expected, actual, prev_num_terry),
  label = c("Expected Value", "Actual Territories", "Prev Num. Territories"),
  color = c("darkblue", actual_color, "darkorange")  # Custom colors for each label
)

# Density for the normal distribution curve
density_data <- data.frame(
  x = seq(mean - 4 * sd, mean + 4 * sd, length.out = 500),
  y = dnorm(seq(mean - 4 * sd, mean + 4 * sd, length.out = 500), mean = mean, sd = sd)
)

# Plot
ggplot(data, aes(x = values)) +
  # Histogram
  geom_histogram(aes(y = ..density.. * 100), bins = 20, fill = primary_color, color = secondary_color) +
  # Normal distribution curve
  geom_line(data = density_data, aes(x = x, y = y * 100), 
            color = "#54585A", linetype = "solid", size = 0.5, 
            inherit.aes = FALSE) +
  # Vertical lines with legend
  geom_vline(data = vline_data, aes(xintercept = xintercept, color = label), 
             linetype = c("dashed", "dashed", "dashed"), size = 1, show.legend = TRUE) +
  # Labels
  labs(
    title = glue("Number of Territories Histogram: {myteam}"),
    subtitle = glue(
      "*Expected*: {mean}, *Actual*: {actual}, ∆ Territories = {delta}"
    ),
    x = "Number of Territories Won",
    y = "Percent Chance to Win N Territories (%)",
    color = "X ~ N (μ, σ)"
  ) +
  scale_color_manual(
    values = setNames(vline_data$color, vline_data$label)
  ) +
  # Theme adjustments
  theme_bw() +
  theme(
    plot.title = element_markdown(hjust = 0.5),
    plot.subtitle = element_markdown(hjust = 0.5),
    legend.position = c(0.95, 0.95),  # Top-right corner (x, y in [0, 1] coordinates)
    legend.justification = c(1, 1),   # Align legend box to the top-right
    legend.background = element_rect(fill = "white", color = "gray")  # Optional: Add border
  )
