library(ggplot2)
library(ggtext)
library(glue)
mean <- 20
sd <- 5
delta <- 2
expected <- 22
actual <- 25
myteam <- "March"

ggplot(data = data.frame(values = rnorm(1000, mean = mean, sd = sd)), aes(x = values)) +
  geom_histogram(bins = 20, fill = "skyblue", color = "white") +
  labs(
    title = glue("Number of Territories Histogram: {myteam}"),
    subtitle = glue(
      "*Expected*: {mean}, *Actual*: {actual}, Delta *Territories* = {delta}."
    ),
    x = "Number of Territories Won",
    y = "Percent Chance to Win N Territories (%)"
  ) +
  theme_bw() +
  theme(
    plot.subtitle = element_markdown()
  )
