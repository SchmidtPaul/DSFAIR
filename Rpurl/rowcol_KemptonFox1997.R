# packages
pacman::p_load(janitor, readr, tidyverse, # data import and handling
               lme4, lmerTest,            # linear mixed model 
               emmeans, multcomp,         # mean comparisons
               ggplot2, desplot)          # plots 

# data (import via URL)
dataURL <- "https://raw.githubusercontent.com/SchmidtPaul/DSFAIR/master/data/Kempton%26Fox1997.csv"
dat <- read_csv(dataURL)

dat

dat <- dat %>% 
  mutate_at(vars(gen, rep), as.factor)

desplot(data = dat,
        form = gen ~ col + row | rep,            # fill color per genotype, headers per replicate
        text = gen, cex = 0.7, shorten = "no",   # show genotype names per plot
        out1 = row, out1.gpar=list(col="black"), # lines between rows
        out2 = col, out2.gpar=list(col="black"), # lines between columns
        main = "Field layout", show.key = F)     # formatting

dat %>% 
  group_by(gen) %>% 
  summarize(mean    = mean(yield, na.rm=TRUE),
            std.dev = sd(yield, na.rm=TRUE)) %>% 
  arrange(desc(mean)) %>% # sort
  print(n=Inf) # print full table

ggplot(data = dat,
       aes(y = yield, x = gen, color=rep)) +
  geom_point() +  # scatter plot
  ylim(0, NA) +   # force y-axis to start at 0
  theme_classic() + # clearer plot format 
  theme(axis.text.x = element_text(angle=90, vjust=0.5), # rotate x-axis label
        panel.grid.major.x = element_line(), # add vertikal grid lines
        legend.position = "top") # legend on top 

dat <- dat %>% 
  mutate(row_fct = as.factor(row),
         col_fct = as.factor(col))

# rows and cols fixed (linear model)
mod.fb <- lm(yield ~ gen + rep +
               rep:row_fct + 
               rep:col_fct,
             data = dat)

mod.fb %>%
  emmeans(pairwise ~ "gen",
          adjust = "tukey") %>%
  pluck("contrasts") %>% # extract diffs
  as_tibble %>% # format to table
  pluck("SE") %>% # extract s.e.d. column
  mean() # get arithmetic mean

# rows and cols random (linear mixed model)
mod.rb <- lmer(yield ~ gen + rep +
                 (1|rep:row_fct) + 
                 (1|rep:col_fct),
               data = dat)

mod.rb %>%
  emmeans(pairwise ~ "gen",
          adjust = "tukey",
          lmer.df = "kenward-roger") %>%
  pluck("contrasts") %>% # extract diffs
  as_tibble %>% # format to table
  pluck("SE") %>% # extract s.e.d. column
  mean() # get arithmetic mean

mod.rb %>% anova(ddf="Kenward-Roger")

mean_comparisons <- mod.rb %>%
  emmeans(pairwise ~ "gen",
          adjust = "tukey",
          lmer.df = "kenward-roger") %>%
  pluck("emmeans") %>%
  cld(details = TRUE, Letters = letters) # add letter display

mean_comparisons$emmeans # adjusted genotype means

# sort gen factors according to emmean
# for adjusted means
mean_comparisons$emmeans <- mean_comparisons$emmeans %>% 
  mutate(gen = fct_reorder(.f = gen, .x = emmean))

# for raw data
dat <- dat %>% 
  mutate(gen = fct_relevel(.f = gen, 
                           as.character(mean_comparisons$emmeans$gen)))

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
  theme(axis.text.x = element_text(angle=90, vjust=0.5)) # rotate x-axis label

# data (import via URL)
dataURL <- "https://raw.githubusercontent.com/SchmidtPaul/DSFAIR/master/data/RowColFromUtz.csv"
ex1dat <- read_csv(dataURL)
