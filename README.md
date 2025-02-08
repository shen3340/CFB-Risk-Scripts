# CFB Risk Scripts
A simple guide to providing stats/graphs for the game [College Football Risk](https://collegefootballrisk.com/).

# Setup
1. Every single one of these scripts is meant to run in R, and you can follow these steps for downloading R and RStudio (the IDE for R) here: https://posit.co/download/rstudio-desktop
2. There are different packages needed to run the scripts, and you have to install these packages before loading them. For example, if the first package is
```
library(tidyverse)
```
you first need to run this code: 
```
install.packages("tidyverse")
```
# Daily Team Scripts
These scripts will output the following information for your own team for a specific turn: 

•Odds for each team territory

•Legal moves for the following turn

•MVP player shoutouts
# Daily Summary Scripts
This script will output the following information to encompass every team for a specific turn: 

• 5 territories that were won with the lowest win chance

• Histograms for each team showing expected territories and other relevant statistics

• A table of the 3 luckiest and unluckiest teams 

• Shoutout for players who hit turn milestones, e.g turn 50/100 

# Archive

This folder encompasses the luckiest territories and all histograms for every past turn in the game. 

# Team Dashboard

For non-technical individuals, I created a Shiny App that will output all of the information from the Daily Team Script without having to run any code at [College Football Risk Shiny App (currently archived)](https://shen3340.shinyapps.io/Teams/).





