knitr::opts_chunk$set(echo = TRUE,
                      warning = FALSE,
                      message = FALSE)
library(kableExtra)

pacman::p_load(emmeans, modelbased,
               scales,
               tidyverse)

# data from Kozak & Piepho (2020)
dataURL <- "https://raw.githubusercontent.com/SchmidtPaul/DSFAIR/master/data/Kozak%26Piepho2020.csv"
dat <- read_csv(dataURL)

# estimate SD using raw data
rawSDs <- dat %>% 
  group_by(treatment) %>% 
  summarise(rawSD = sd(value))

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
    ) -> CIs[[paste(this_ci)]]
  
}

# join results
result <- plyr::join_all(CIs) %>% left_join(rawSDs)

# transpose results for plot
result_transposed <- result %>%
  mutate(
    SE_low = Mean - SE,
    SE_high = Mean + SE,
    rawSD_low = Mean - rawSD,
    rawSD_high = Mean + rawSD
  ) %>%
  select(-SE,-rawSD) %>%
  pivot_longer(
    cols = -c(treatment, Mean),
    names_to = c("type", "lowhigh"),
    names_pattern = "(.*)_(.*)"
  ) %>%
  pivot_wider(values_from = value,
              names_from = lowhigh) %>%
  mutate(type = fct_reorder(type, low, median))
