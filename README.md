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
## Spreadsheet output
This script was specifically made for one of the users who uses a spreadsheet for their own analysis. It outputs cumulative statistics for each player on their team. 
# Daily Summary Scripts
## Summary Stats
This script is meant to output summary stats used for each turn that help with assisting my daily stats recaps. This script has the 5 luckiest territories and a shoutout for players that hit certain turn milestones, e.g. turn 50/100 etc.
![image](https://github.com/shen3340/CFB-Risk-Scripts/blob/321b3e6ae4ea39cde19c706c2344defa1f284702/Daily%20Summary%20Scripts/Images/5%20Luckiest%20Territories%20Season%205%2C%20Day%204.png)

## Histogram test
These histograms show expected territories won by each team for each roll, using convolution to visualize the probability distribution. 
![image](https://github.com/shen3340/CFB-Risk-Scripts/blob/ddedd2cd4d73d7fa302153d0eb1ba58a6d58b340/Archive/Month%20Risk/Day%2027/1_March.png)






