#! /usr/bin/env Rscript
# Plot equivalent 2k times based on adjusted weight

library(ggplot2)
library(lubridate)
library(scales)

adj <- function(actual, base) (actual/base)^0.222
weight <- seq(50, 110, 1)
time <- hms::as_hms(seq(6 * 60, 7.5 * 60, 10))

df <- expand.grid(weight = weight, time = time)
df$adj_time <- df$time/adj(df$weight, 85)
df$group = as.factor(df$time)

p <- ggplot(
    df, aes(
        weight, hms::as_hms(adj_time),
        group = group, color = group
    )
) +
    geom_line() + labs(
    x = "weight [kg]", y = "2k time [m:s]", 
    title = "Ergometer times for 2000m adjusted by body weight",
    caption = "Lines indicate equivalent times accross body weights"
) +
    scale_y_time(breaks = scales::breaks_width("10 sec")) +
    scale_x_continuous(
        breaks = scales::breaks_width(5),
        sec.axis = sec_axis(~(. * 2.205), 
        name = "weight [lb]", 
        breaks = scales::breaks_width(10))
    )
ggsave("ergo.png", p)

