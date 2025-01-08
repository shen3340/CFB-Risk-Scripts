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
These scripts are meant to use for your own specific teams. 
## MVP
This script is meant to output a shoutout to all MVPs for a given turn. 
## Odds
This script is meant to output the numerical odds of winning each territory, with an output of which teams attacked/defended that territory.
![image](https://github.com/user-attachments/assets/1483234f-9a53-4aa4-aac6-bf135e027b52)
## Legal Moves
This script is meant to output every single territory that your team is legally able to attack or defend, making it easier to upload orders from this text file. There is a weird thing with this script that requires the day to be one head of the actual game roll.
# Daily Summary Scripts
## Summary Stats
This script is meant to output the 5 territories that were won with the lowest chance of winning. This script will eventually have more summary stats than just the top 5 luckiest territories.

## Histogram test
This is a work in progress as of now, but it will eventually show a histogram of expected territories won by each team for each roll, using a Monte Carlo Simulation.






