knitr::opts_chunk$set(echo = TRUE,
                      warning = FALSE,
                      message = FALSE)

# packages
pacman::p_load(tidyverse) # data import and handling

# data (import via URL)
dataURL <- "https://raw.githubusercontent.com/SchmidtPaul/DSFAIR/master/data/DrinksPeterMax.csv"
dat <- read_csv(dataURL)

dat

summary(dat)
plot(y = dat$blood_alc, x = dat$drinks)

knitr::include_graphics("img/correlation.PNG")
