pacman::p_load(emmeans, multcomp, multcompView,
               scales,
               tidyverse)

model <- lm(weight ~ group, data = PlantGrowth)

model_means <- emmeans::emmeans(object = model,
                                pairwise ~ "group",
                                adjust = "tukey")

model_means_cld <- multcomp::cld(object = model_means$emmeans,
                                 Letters = letters) %>% 
  mutate(group = fct_reorder(group, emmean))

model_means_cld %>%
  as_tibble() %>%
  transmute(
    Group = str_to_title(group),
    `Mean weight*` = paste0(
      number(emmean, accuracy = 0.1),
      "<sup>",
      str_trim(.group),
      "</sup>"
    )
  ) %>%
  kbl(escape = F) %>% 
  kable_classic_2("hover", position = "left", full_width = FALSE)


model_means_cld %>%
  mutate(group = fct_reorder(group, emmean)) %>%
  ggplot(data = .,
         aes(
           y = emmean,
           x = group,
           ymin = lower.CL,
           ymax = upper.CL,
           label = str_trim(.group)
         )) +
  geom_point() +
  geom_errorbar(width = 0.1) +
  geom_text(position = position_nudge(x = 0.1), hjust = 0) +
  scale_y_continuous(
    name = "Mean weight\n± 95% confidence interval",
    limits = c(0, NA),
    breaks = pretty_breaks(),
    expand = expansion(mult = c(0, 0.05))
  ) +
  scale_x_discrete(
    name = NULL,
    labels = function(x)
      str_to_title(x)
  ) +
  labs(caption = "Means not sharing any letter are significantly different\nby the Tukey-test at the 5% level of significance.") +
  theme_classic()

library(emmeans)
library(multcomp)
library(multcompView)

# set up model
model <- lm(weight ~ group, data = PlantGrowth)

# get (adjusted) weight means per group
model_means <- emmeans(object = model,
                       specs = "group")

# add letters to each mean
model_means_cld <- cld(object = model_means,
                       adjust = "Tukey",
                       Letters = letters,
                       alpha = 0.05)

# show output
model_means_cld

library(tidyverse) # ggplot & helper functions
library(scales)    # more helper functions

# optional: sort factor levels of groups column according to highest mean

# ...in means table
model_means_cld <- model_means_cld %>% 
  mutate(group = fct_reorder(group, emmean))

# ...in data table
PlantGrowth <- PlantGrowth %>% 
  mutate(group = fct_relevel(group, levels(model_means_cld$group)))

# base plot setup
ggplot() +
  # y-axis
  scale_y_continuous(
    name = "Weight",
    limits = c(0, NA),
    breaks = pretty_breaks(),
    expand = expansion(mult = c(0,0.1))
  ) +
  # x-axis
  scale_x_discrete(
    name = "Treatment"
  ) +
  # general layout
  theme_classic() +
  # black data points
  geom_point(
    data = PlantGrowth,
    aes(y = weight, x = group),
    shape = 16,
    alpha = 0.5,
    position = position_nudge(x = -0.2)
  ) +
  # black boxplot
  geom_boxplot(
    data = PlantGrowth,
    aes(y = weight, x = group),
    width = 0.05,
    outlier.shape = NA,
    position = position_nudge(x = -0.1)
  ) +
  # red mean value
  geom_point(
    data = model_means_cld,
    aes(y = emmean, x = group),
    size = 2,
    color = "red"
  ) +
  # red mean errorbar
  geom_errorbar(
    data = model_means_cld,
    aes(ymin = lower.CL, ymax = upper.CL, x = group),
    width = 0.05,
    color = "red"
  ) +
  # red letters
  geom_text(
    data = model_means_cld,
    aes(
      y = emmean,
      x = group,
      label = str_trim(.group)
    ),
    position = position_nudge(x = 0.1),
    hjust = 0,
    color = "red"
  ) +
  # caption
  labs(
    caption = str_wrap("Black dots represent raw data. Red dots and error bars represent (estimated marginal) means ± 95% confidence interval per group. Means not sharing any letter are significantly different by the Tukey-test at the 5% level of significance.", width = 70)
  )

library(tidyverse) # ggplot & helper functions
library(scales)    # more helper functions

# optional: sort factor levels of groups column according to highest mean

# ...in means table
model_means_cld <- model_means_cld %>% 
  mutate(group = fct_reorder(group, emmean))

# ...in data table
PlantGrowth <- PlantGrowth %>% 
  mutate(group = fct_relevel(group, levels(model_means_cld$group)))

# base plot setup
ggplot() +
  # y-axis
  scale_y_continuous(
    name = "Weight",
    limits = c(0, NA),
    breaks = pretty_breaks(),
    expand = expansion(mult = c(0,0.1))
  ) +
  # x-axis
  scale_x_discrete(
    name = "Treatment"
  ) +
  # general layout
  theme_classic() +
    # bars
  geom_bar(data = model_means_cld,
           aes(y = emmean, x = group),
           stat = "identity") +
  # errorbars
  geom_errorbar(data = model_means_cld,
                aes(
                  ymin = emmean - SE,
                  ymax = emmean + SE,
                  x = group
                ),
                width = 0.1) +
  # letters
  geom_text(
    data = model_means_cld,
    aes(
      y = emmean + SE,
      x = group,
      label = str_trim(.group)
    ),
    hjust = 0.5,
    vjust = -0.5
  ) +
  # caption
  labs(
    caption = str_wrap("Bars with errorbars represent (estimated marginal) means ± standard error. Means not sharing any letter are significantly different by the Tukey-test at the 5% level of significance.", width = 70)
  )

pwpp(model_means) + theme_bw()

library(modelbased)
library(see)
plot(estimate_contrasts(model, adjust = "tukey"),
     estimate_means(model)) +
  theme_classic()

library(ggstatsplot)
# "since the confidence intervals for the effect sizes are computed using
# bootstrapping, important to set a seed for reproducibility"
set.seed(42)
ggstatsplot::ggbetweenstats(
  data = PlantGrowth,
  x = group,
  y = weight,
  pairwise.comparisons = TRUE,
  pairwise.display = "all",
  p.adjust.method = "none"
)
