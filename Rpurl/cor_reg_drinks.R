knitr::opts_chunk$set(echo = TRUE,
                      warning = FALSE,
                      message = FALSE)

# packages
pacman::p_load(
  modelbased, # get model predictions/expectations
  tidyverse # data import and handling
)

# data (import via URL)
dataURL <- "https://raw.githubusercontent.com/SchmidtPaul/DSFAIR/master/data/DrinksPeterMax.csv"
dat <- read_csv(dataURL)

dat

summary(dat)
plot(y = dat$blood_alc, x = dat$drinks)

ggplot(data = dat, aes(x = drinks, y = blood_alc)) +
  geom_point(size = 2) +
  scale_x_continuous(
    name = "Number of drinks",
    limits = c(0, 9),
    breaks = seq(0, 9)
  ) +
  scale_y_continuous(
    name = "Blood alcohol content",
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.1))
  ) +
    theme_classic()

knitr::include_graphics("img/correlation.PNG")

cor(dat$drinks, dat$blood_alc)

cor.test(dat$drinks, dat$blood_alc)

library(scales)
cor <- cor.test(dat$drinks, dat$blood_alc)

knitr::include_graphics("img/regressionexamples.png")

knitr::include_graphics("img/regressiontheory.png")

reg <- lm(formula = blood_alc ~ drinks, data = dat)

reg

ggplot(data = dat, aes(x = drinks, y = blood_alc)) +
  geom_point(size = 2) +
  geom_smooth(
    method = "lm",
    formula = "y ~ x",
    se = FALSE,
    fullrange = TRUE,
    color = "#769bbb"
  ) +
  scale_x_continuous(
    name = "Number of drinks",
    limits = c(0, 9),
    breaks = seq(0, 9)
  ) +
  scale_y_continuous(
    name = "Blood alcohol content",
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.1))
  ) +
  theme_classic()

preddat <- data.frame(drinks = seq(0, 3))
estimate_expectation(model = reg, data = preddat)

summary(reg)

reg_noint <- lm(formula = blood_alc ~ 0 + drinks, data = dat)
reg_noint

ggplot(data = dat, aes(x = drinks, y = blood_alc)) +
  geom_point(size = 2) +
  geom_smooth(
    method = "lm",
    formula = "y ~ x",
    se = FALSE,
    fullrange = TRUE,
    color = "#769bbb"
  ) +
    geom_smooth(
    method = "lm",
    formula = "y ~ 0 + x",
    se = FALSE,
    fullrange = TRUE,
    color = "#DF9A57"
  ) +
  scale_x_continuous(
    name = "Number of drinks",
    limits = c(0, 9),
    breaks = seq(0, 9)
  ) +
  scale_y_continuous(
    name = "Blood alcohol content",
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.1))
  ) +
  theme_classic()
