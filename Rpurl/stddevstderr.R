knitr::opts_chunk$set(echo = TRUE,
                      warning = FALSE,
                      message = FALSE)
library(kableExtra)

pacman::p_load(modelbased,
               tidyverse)

# data from Kozak & Piepho (2020)
dataURL <- "https://raw.githubusercontent.com/SchmidtPaul/DSFAIR/master/data/Kozak%26Piepho2020.csv"
dat <- read_csv(dataURL)

# estimate SD using raw data
rawSDs <- dat %>% 
  group_by(treatment) %>% 
  summarise(
    rawSD = sd(value))

# set up model
model <- lm(value ~ treatment + block, data = dat)

# get CIs
CIs <- list()

for (this_ci in c(0.8, 0.95, 0.99)) {
  model %>%
    estimate_means(levels = "treatment", ci = this_ci) %>%
    as_tibble() %>%
    rename_at(
      .vars = vars(CI_low, CI_high),
      .funs = function(x)
        str_replace(x, "_", paste0(this_ci * 100, "_"))
    ) %>% 
    rename(SEM = SE) -> CIs[[paste(this_ci)]]
  
}

# join results
result <- plyr::join_all(CIs) %>% left_join(rawSDs)

# transpose results for plot
result_transposed <- result %>%
  mutate(
    SEM_low = Mean - SEM,
    SEM_high = Mean + SEM,
    rawSD_low = Mean - rawSD,
    rawSD_high = Mean + rawSD
  ) %>%
  select(-SEM,-rawSD) %>%
  pivot_longer(
    cols = -c(treatment, Mean),
    names_to = c("type", "lowhigh"),
    names_pattern = "(.*)_(.*)"
  ) %>%
  pivot_wider(values_from = value,
              names_from = lowhigh) %>%
  mutate(type = fct_reorder(type, low, median))

ggplot(data = result_transposed,
       aes(x = treatment, y = Mean, ymin = low, ymax = high)) +
  facet_grid(cols = vars(type)) + 
  geom_point() +
  geom_errorbar(width = 0.1) +
  labs(y = "Mean ± ...", x = NULL) +
  theme_bw()

p1 <- ggplot(data = filter(result_transposed, treatment == "A1"),
             aes(
               x = type,
               y = Mean,
               ymin = low,
               ymax = high
             )) +
  geom_point() +
  geom_errorbar(width = 0.1) +
  scale_y_continuous(
    name = "A1 Mean ± ...",
    limits = c(70, 130),
    breaks = scales::pretty_breaks()) +
  xlab(NULL) +
  theme_bw()

p2 <- ggplot(data = filter(dat, treatment == "A1"),
             aes(y = value, x = treatment)) +
  geom_point(shape = 21) +
    scale_y_continuous(
    name = "Raw Data",
    limits = c(70, 130),
    breaks = scales::pretty_breaks()) +
  geom_boxplot(position = position_nudge(x = 0.3), width = 0.2, color = "darkgrey") +
  xlab(NULL) +
  theme_bw()

cowplot::plot_grid(p2, p1, nrow = 1, rel_widths = c(2,5), align = "h")
