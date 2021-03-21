# packages
pacman::p_load(agridat, tidyverse, # data import and handling
               conflicted,         # handling function conflicts
               emmeans, multcomp,  # mean comparisons
               ggplot2, desplot)   # plots

# conflicts: identical function names from different packages
conflict_prefer("filter", "dplyr")
conflict_prefer("select", "dplyr")

# data (from agridat package)
dat <- agridat::bridges.cucumber %>% 
  filter(loc == "Clemson") %>% # subset data from only one location
  select(-loc) # remove loc column which is now unnecessary

dat

dat <- dat %>% 
  as_tibble() %>% # tibble data format for convenience
  mutate(rowF = row %>% as.factor,
         colF = col %>% as.factor)

desplot(data = dat, flip = TRUE,
        form = gen ~ row + col, 
        out1 = row, out1.gpar=list(col="black", lwd=3),
        out2 = col, out2.gpar=list(col="black", lwd=3),
        text = gen, cex = 1, shorten = "no",
        main = "Field layout", 
        show.key = FALSE)

dat %>% 
  group_by(gen) %>% # genotype
  summarize(mean    = mean(yield),
            std.dev = sd(yield)) %>% 
  arrange(desc(mean))

dat %>% 
  group_by(row) %>% # row
  summarize(mean    = mean(yield),
            std.dev = sd(yield))

dat %>% 
  group_by(col) %>% # column
  summarize(mean    = mean(yield),
            std.dev = sd(yield))

ggplot(data = dat,
       aes(y = yield, x = gen, color = rowF, shape=colF)) +
  geom_point(size=2) +  # scatter plot with larger points
  ylim(0, NA) +   # force y-axis to start at 0
  theme_classic() # clearer plot format 

mod <- lm(yield ~ gen + rowF + colF, data = dat)

mod %>% anova()

mean_comparisons <- mod %>% 
  emmeans(pairwise ~ "gen", adjust="tukey") %>% # adjust="none" for t-test
  pluck("emmeans") %>% 
  cld(details=TRUE, Letters=letters) # add letter display

mean_comparisons$emmeans # adjusted genotype means

mean_comparisons$comparisons # differences between adjusted genotype means 

ggplot() +
  # black dots representing the raw data
  geom_point(
    data = dat,
    aes(y = yield, x = gen)
  ) +
  # red dots representing the adjusted means
  geom_point(
    data = mean_comparisons$emmeans,
    aes(y = emmean, x = gen),
    color = "red",
    position = position_nudge(x = 0.1)
  ) +
  # red error bars representing the confidence limits of the adjusted means
  geom_errorbar(
    data = mean_comparisons$emmeans,
    aes(ymin = lower.CL, ymax = upper.CL, x = gen),
    color = "red",
    width = 0.1,
    position = position_nudge(x = 0.1)
  ) +
  # red letters 
  geom_text(
    data = mean_comparisons$emmeans,
    aes(y = emmean, x = gen, label = .group),
    color = "red",
    position = position_nudge(x = 0.2)
  ) + 
  ylim(0, NA) + # force y-axis to start at 0
  ylab("Yield") + # label y-axis
  xlab("Cucumber genotype") + # label x-axis
  labs(caption = "Black dots represent raw data
       Red dots and error bars represent adjusted mean with 95% confidence limits per genotype
       Means followed by a common letter are not significantly different according to the Tukey-test") +
  theme_classic() # clearer plot format 
