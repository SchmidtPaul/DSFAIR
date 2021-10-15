# packages
pacman::p_load(readr, tidyverse, # data import and handling
               conflicted,       # handling function conflicts
               lme4, lmerTest,   # linear mixed model 
               emmeans, multcomp, multcompView, # mean comparisons
               ggplot2, desplot) # plots

# conflicts: identical function names from different packages
conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("lmer", "lmerTest")

# data (import via URL)
dataURL <- "https://raw.githubusercontent.com/SchmidtPaul/DSFAIR/master/data/Gomez%26Gomez1984.csv"
dat <- read_csv(dataURL)

dat

dat <- dat %>% 
  mutate_at(vars(rep:N), as.factor)

desplot(data = dat,
        form = rep ~ col + row | rep, # fill color per rep, headers per rep
        text = G, cex = 1, shorten = "no", # show genotype names per plot
        col  = N, # color of genotype names for each N-level
        out1 = mainplot, out1.gpar = list(col = "black"), # lines between mainplots
        out2 = row, out2.gpar = list(col = "darkgrey"), # lines between rows
        main = "Field layout", show.key = TRUE, key.cex = 0.7) # formatting

dat %>% 
  group_by(G) %>% 
  summarize(mean    = mean(yield),
            std.dev = sd(yield)) %>% 
  arrange(desc(mean)) %>% # sort
  print(n=Inf) # print full table

dat %>% 
  group_by(N) %>% 
  summarize(mean    = mean(yield),
            std.dev = sd(yield))  %>% 
  arrange(desc(mean)) %>% # sort
  print(n=Inf) # print full table

ggplot(data = dat, 
       aes(y = yield, 
           x = N,
           color = N)) +
  facet_grid(~G) + # facette per N level
  geom_point() +  # scatter plot observed
  theme_bw() + # clearer plot format 
  theme(legend.position = "top") # legend on top

mod <- lmer(yield ~ G + N + G:N + 
              rep + (1|rep:mainplot), 
            data=dat)

mod %>% 
  VarCorr() %>% 
  as.data.frame() %>% 
  select(grp, vcov)

mod %>% anova(ddf="Kenward-Roger")

all_mean_comparisons <- mod %>%
  emmeans(specs = ~ N * G, 
          lmer.df = "kenward-roger") %>% 
  cld(adjust="tukey", Letters=letters) # add compact letter display

all_mean_comparisons # adjusted means

withinG_mean_comparisons <- mod %>%
  emmeans(specs = ~ N | G,
          lmer.df = "kenward-roger") %>% 
  cld(adjust="tukey", Letters=letters) # add compact letter display

withinG_mean_comparisons # adjusted means

# reformatting needed for ggplot
withinG_mean_comparisons <- as_tibble(withinG_mean_comparisons)

ggplot() +
  facet_grid(~G) +
  # black dots representing the raw data
  geom_point(
    data = dat,
    aes(y = yield, x = N)
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
    aes(y = lower.CL, x = N, label = .group),
    color = "red",
    angle = 90,
    hjust = 1,
    position = position_nudge(y = - 1)
  ) + 
  ylim(0, NA) + # force y-axis to start at 0
  ylab("Yield in t/ha") + # label y-axis
  xlab("Nitrogen Level") +      # label x-axis
  labs(caption = "The four facettes represent genotypes A, B, C and D
       Black dots represent raw data
       Red dots and error bars represent adjusted mean with 95% confidence limits per genotype-nitrogen level combination
       Separaetly per genotype, means followed by a common letter are not significantly different according to the Tukey-test") +
  theme_bw() + # clearer plot format 
  theme(axis.text.x = element_text(angle=90, vjust=0.5)) # rotate x-axis label
