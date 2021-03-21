# packages
pacman::p_load(tidyverse,         # data import and handling
               conflicted,        # handling function conflicts
               lme4, lmerTest,    # linear mixed model 
               emmeans, multcomp, multcompView, # mean comparisons
               ggplot2, desplot)  # plots

# conflicts: identical function names from different packages
conflict_prefer("select", "dplyr")
conflict_prefer("lmer", "lmerTest")

# data (import via URL)
dataURL <- "https://raw.githubusercontent.com/SchmidtPaul/DSFAIR/master/data/John%26Williams1995.csv"
dat <- read_csv(dataURL)

dat

dat <- dat %>% 
  mutate_at(vars(plot:gen), as.factor)

desplot(data = dat, flip = TRUE,
        form = gen ~ col + row | rep,          # fill color per genotype, headers per replicate
        text = gen, cex = 0.7, shorten = "no", # show genotype names per plot
        out1 = rep,                            # lines between complete blocks/replicates
        out2 = inc.block,                      # lines between incomplete blocks
        main = "Field layout", show.key = F)   # formatting

dat %>% 
  group_by(gen) %>% 
  summarize(mean    = mean(yield),
            std.dev = sd(yield)) %>% 
  arrange(desc(mean)) %>% # sort
  print(n=Inf) # print full table

dat %>% 
  group_by(rep, inc.block) %>% 
  summarize(mean    = mean(yield),
            std.dev = sd(yield))  %>% 
  arrange(desc(mean)) %>% # sort
  print(n=Inf) # print full table

plotdata <- dat %>% 
  group_by(gen) %>% 
  mutate(mean_yield = mean(yield)) %>% # add column with mean yield per gen
  ungroup() %>% 
  mutate(gen = fct_reorder(.f = gen, .x = mean_yield)) # sort factor variable by mean yield

ggplot(data = plotdata, 
       aes(x = gen)) +
  geom_point(aes(y = yield, shape = rep)) +  # scatter plot observed
  geom_point(aes(y = mean_yield), color = "cornflowerblue") + # scatter plot mean
  ylim(0, NA) +   # force y-axis to start at 0
  labs(caption = "Blue dots represent arithmetic mean per genotype") +
  theme_classic() + # clearer plot format 
  theme(axis.text.x = element_text(angle=90, vjust=0.5)) # rotate x-axis labels

# blocks as fixed (linear model)
mod.fb <- lm(yield ~ gen + rep +
               rep:inc.block,
             data = dat)

mod.fb %>%
  emmeans(pairwise ~ "gen",
          adjust = "tukey") %>%
  pluck("contrasts") %>% # extract diffs
  as_tibble %>% # format to table
  pull("SE") %>% # extract s.e.d. column
  mean() # get arithmetic mean

# blocks as random (linear mixed model)
mod.rb <- lmer(yield ~ gen + rep +
                 (1 | rep:inc.block),
               data = dat)

mod.rb %>%
  emmeans(pairwise ~ "gen",
          adjust = "tukey",
          lmer.df = "kenward-roger") %>%
  pluck("contrasts") %>% # extract diffs
  as_tibble %>% # format to table
  pull("SE") %>% # extract s.e.d. column
  mean() # get arithmetic mean

mod.rb %>% 
  VarCorr() %>% 
  as.data.frame() %>% 
  select(grp, vcov)

mod.rb %>% anova(ddf="Kenward-Roger")

mean_comparisons <- mod.rb %>%
  emmeans(pairwise ~ "gen",
          adjust = "tukey",
          lmer.df = "kenward-roger") %>%
  pluck("emmeans") %>%
  cld(details = TRUE, Letters = letters) # add letter display

# If cld() does not work, try CLD() instead.
# Add 'adjust="none"' to the emmeans() and cld() statement
# in order to obtain t-test instead of Tukey!

mean_comparisons$emmeans # adjusted genotype means

ggplot() +
  # black dots representing the raw data
  geom_point(
    data = plotdata,
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
    aes(y = lower.CL, x = gen, label = .group),
    color = "red",
    angle = 90,
    hjust = 1,
    position = position_nudge(y = - 0.1)
  ) + 
  ylim(0, NA) + # force y-axis to start at 0
  ylab("Yield in t/ha") + # label y-axis
  xlab("Genotype") +      # label x-axis
  labs(caption = "Black dots represent raw data
       Red dots and error bars represent adjusted mean with 95% confidence limits per genotype
       Means followed by a common letter are not significantly different according to the Tukey-test") +
  theme_classic() + # clearer plot format 
  theme(axis.text.x = element_text(angle=90, vjust=0.5)) # rotate x-axis
