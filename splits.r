#! /usr/bin/env Rscript

library(lubridate)

adj <- function(actual, base) (actual/base)^0.222
pwr <- function(spd) 2.8 * spd^3
spd <- function(pwr) (pwr/2.8)^(1/3)

round_df <- function(df, digits) {
  nums <- vapply(df, is.numeric, FUN.VALUE = logical(1))
  df[,nums] <- round(df[,nums], digits = digits)
  (df)
}

ms <- function(s) {
  t = seconds_to_period(s)
  sprintf('%02d:%04.1f', minute(t), second(t))
}

time2k <- seq(6*60, 8.5*60,5)
spd2k <- 2000/time2k
pwr2k <- pwr(spd2k)

time5k <- time2k * (5000/2000)^1.07
splt5k <- (500*time5k/5000)

dst30r20 <- 2000 * (30*60/time2k)^(1/1.07)
splt30r20 <- 500 * 30*60 / dst30r20

splt2k <- (time2k/4)
splt55 <- (500 / spd(pwr2k * 0.55))
splt65 <- (500 / spd(pwr2k * 0.65))
splt75 <- (500 / spd(pwr2k * 0.75))

t=splt30r20

df <-data.frame(
                ms(time2k),
                ms(splt2k), 
                (splt2k-t), 
                ms(time5k),
                ms(splt5k),
                (splt5k-t),
                ms(splt30r20),
                ms(splt65),
                (splt65-t),
                ms(splt55),
                (splt55-t))
colnames(df) <- c("2k", 
                  "2k/500",
                  "2k/500", 
                  "5k",
                  "5k/500", 
                  "5k/500", 
                  "30R20/500", 
                  "UT1/500",
                  "UT1/500", 
                  "UT2/500",
                  "UT2/500")
df <- round_df(df,1)

