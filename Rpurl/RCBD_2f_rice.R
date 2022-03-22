knitr::opts_chunk$set(echo = TRUE,
                      warning = FALSE,
                      message = FALSE)

# packages
pacman::p_load(tidyverse, # data import and handling
               conflicted, # handling function conflicts
               emmeans, multcomp, multcompView, # adjusted mean comparisons
               ggplot2, desplot) # plots

# data (import via URL)
dataURL <- "https://raw.githubusercontent.com/SchmidtPaul/DSFAIR/master/data/riceRCBD.csv"
dat <- read_csv(dataURL)

dat

dat <- dat %>% 
  mutate_at(vars(rep, N, G), as.factor)

desplot(data = dat,
        form = rep ~ col + row | rep, # fill color per rep, headers per rep
        text = G, cex = 1, shorten = "no", # show genotype names per plot
        col  = N, # color of genotype names for each N-level
        out1 = col, out1.gpar = list(col = "darkgrey"), # lines between columns
        out2 = row, out2.gpar = list(col = "darkgrey"), # lines between rows
        main = "Field layout", show.key = TRUE, key.cex = 0.7) # formatting

dat %>% 
  group_by(G) %>% 
  summarize(mean    = mean(yield),
            std.dev = sd(yield)) %>% 
  arrange(desc(mean))

dat %>% 
  group_by(N) %>% 
  summarize(mean    = mean(yield),
            std.dev = sd(yield)) %>% 
  arrange(desc(mean))

dat %>% 
  group_by(N, G) %>% 
  summarize(mean    = mean(yield),
            std.dev = sd(yield)) %>% 
  arrange(desc(mean)) %>% 
  print(n=Inf) # show more than default 10 rows

ggplot(
  data = dat,
  aes(
    y = yield,
    x = N,
    color = N
  )
) +
  facet_wrap(~G, labeller = label_both) + # facette per G level
  geom_point() + # dots representing the raw data
  scale_y_continuous(
    limits = c(0, NA), # make y-axis start at 0
    expand = expansion(mult = c(0, 0.1)) # no space below 0
  ) +
  scale_x_discrete(name = NULL) + # x-axis
  theme_bw() + # clearer plot format
  theme(legend.position = "bottom") # legend on top

mod <- lm(
  yield ~ N + G + N:G + rep,
  data = dat
)

anova(mod)

all_mean_comparisons <- mod %>%
  emmeans(specs = ~ N:G) %>% # compare all combs to all other combs
  cld(adjust = "none", Letters = letters) # add compact letter display

all_mean_comparisons

all_mean_comparisons <- all_mean_comparisons %>% 
  as_tibble() %>%
  mutate(N_G = paste0(N, "-", G)) %>% # create helper column with combs
  mutate(N_G = fct_reorder(N_G, emmean)) # sort combs according to emmean

# do the same for the raw data
dat <- dat %>% 
  mutate(N_G = paste0(N, "-", G)) %>% 
  mutate(N_G = fct_relevel(N_G, levels(all_mean_comparisons$N_G)))


ggplot() +
  # dots representing the raw data
  geom_point(
    data = dat,
    aes(y = yield, x = N_G, color = N),
    position = position_nudge(x = -0.2)
  ) +
  # black boxplot
  geom_boxplot(
    data = dat,
    aes(y = yield, x = N_G),
    width = 0.05,
    outlier.shape = NA,
    position = position_nudge(x = -0.1)
  ) +
  # red mean value
  geom_point(
    data = all_mean_comparisons,
    aes(y = emmean, x = N_G),
    size = 2,
    color = "red"
  ) +
  # red mean errorbar
  geom_errorbar(
    data = all_mean_comparisons,
    aes(ymin = lower.CL, ymax = upper.CL, x = N_G),
    width = 0.05,
    color = "red"
  ) +
  # red letters
  geom_text(
    data = all_mean_comparisons,
    aes(
      y = 10000,
      x = N_G,
      label = .group
    ),
    angle = 90,
    hjust = 0,
    color = "red"
  ) +
  # y-axis
  scale_y_continuous(
    name = "Yield",
    limits = c(0, 12500),
    expand = expansion(mult = c(0, 0.1))
  ) +
  # x-axis
  scale_x_discrete(name = "Nitrogen-Genotype combination") +
  # general layout
  theme_bw() +
  theme(
    axis.text.x = element_text(
      angle = 45,
      hjust = 1,
      vjust = 1
    ),
    legend.position = "bottom"
  ) +
  labs(
    caption = str_wrap("Black dots represent raw data. Red dots and error bars represent estimated marginal means ± 95% confidence interval per group. Means not sharing any letter are significantly different by the t-test at the 5% level of significance.", width = 120)
  )

withinG_mean_comparisons <- mod %>%
  emmeans(specs = ~ N|G) %>% 
  cld(adjust="none", Letters=letters) # add compact letter display

withinG_mean_comparisons 

withinG_mean_comparisons <- as_tibble(withinG_mean_comparisons)

ggplot() +
  facet_wrap(~G, labeller = label_both) + # facette per G level
  # dots representing the raw data
  geom_point(
    data = dat,
    aes(y = yield, x = N, color = N)
  ) +
  # red dots representing the adjusted means
  geom_point(
    data = withinG_mean_comparisons,
    aes(y = emmean, x = N),
    color = "red",
    position = position_nudge(x = 0.1)
  ) +
  # red error bars representing the confidence limits of the adjusted means
  geom_errorbar(
    data = withinG_mean_comparisons,
    aes(ymin = lower.CL, ymax = upper.CL, x = N),
    color = "red",
    width = 0.1,
    position = position_nudge(x = 0.1)
  ) +
  # red letters
  geom_text(
    data = withinG_mean_comparisons,
    aes(y = emmean, x = N, label = str_trim(.group)),
    color = "red",
    hjust = 0,
    position = position_nudge(x = 0.2)
  ) +
  # y-axis
  scale_y_continuous(
    name = "Yield",
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.1))
  ) +
  # x-axis
  scale_x_discrete(name = NULL) +
  # general layout
  theme_bw() + # clearer plot format
  theme(legend.position = "bottom") + # legend on top
  labs(caption = str_wrap("The four facettes represent genotypes A, B, C and D. Black dots represent raw data. Red dots and error bars represent estimated marginal means ± 95% confidence interval per group. For each genotype separately, means not sharing any letter are significantly different by the t-test at the 5% level of significance.", width = 120))
