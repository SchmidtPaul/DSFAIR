# packages
pacman::p_load(tidyverse,         # data import and handling
               conflicted,        # handling function conflicts
               lme4, lmerTest,    # linear mixed model 
               emmeans, multcomp, multcompView, # adjusted mean comparisons
               ggplot2, desplot, see) # plots

# conflicts: identical function names from different packages
conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("lmer", "lmerTest")

# data (import via URL)
dataURL <- "https://raw.githubusercontent.com/SchmidtPaul/DSFAIR/master/data/Piepho1997.csv"
dat <- read_csv(dataURL)

dat

dat <- dat %>% 
  mutate_at(vars(treat, block, plant), as.factor) %>% 
  mutate(treat = fct_relevel(treat, c("Ctrl", "Top6", "Top1")))

treat_colors <- c(Ctrl="seagreen", Top6="seagreen3" , Top1="seagreen2")

desplot(data = dat, flip = TRUE,
  form = treat ~ col + row | block,
  col.regions = treat_colors,
  main = "Trial layout", show.key = TRUE)

treat_colors <- c(Ctrl="seagreen", Top6="seagreen3" , Top1="seagreen2")

desplot(data = dat, flip = TRUE,
  form = treat ~ col + row | block,
  text = plant, cex = 1,
  out1 = treat, 
  out2 = plant, out2.gpar=list(col="darkgrey"),
  col.regions = treat_colors,
  main = "Trial layout", show.key = TRUE)

dat %>% 
  group_by(treat) %>% 
  summarize(mean    = mean(TKW, na.rm = TRUE),
            std.dev = sd(TKW, na.rm = TRUE),
            n_missing  = sum(is.na(TKW))) %>% 
  arrange(desc(mean)) %>% # sort
  print(n=Inf) # print full table

ggplot(data = dat, 
       aes(y=TKW, x=treat, fill=treat)) +
  geom_violindot(fill_dots = "black", size_dots = 15) +
  scale_fill_manual(name = "Treatment", values = treat_colors) +
  ylim(0, NA) +
  theme_modern()

mod <- lmer(TKW ~ treat + block + (1|block:treat),
            data=dat)

mod %>% 
  VarCorr() %>% 
  as.data.frame() %>% 
  select(grp, vcov)

mod %>% anova(ddf="Kenward-Roger")

mean_comparisons <- mod %>% 
  emmeans(specs = "treat",
          lmer.df = "kenward-roger") %>% # get adjusted means for cultivars
  cld(adjust="tukey", Letters=letters) # add compact letter display

mean_comparisons

ggplot() +
  # black dots representing the raw data
  geom_violindot(
    data = dat,
    aes(y = TKW, x = treat, fill = treat),
    fill_dots = "black",
    size_dots = 15
  ) +
  # red dots representing the adjusted means
  geom_point(
    data = mean_comparisons,
    aes(y = emmean, x = treat),
    color = "red",
    position = position_nudge(x = - 0.2)
  ) +
  # red error bars representing the confidence limits of the adjusted means
  geom_errorbar(
    data = mean_comparisons,
    aes(ymin = lower.CL, ymax = upper.CL, x = treat),
    color = "red",
    width = 0.1,
    position = position_nudge(x = - 0.2)
  ) +
  # red letters 
  geom_text(
    data = mean_comparisons,
    aes(y = emmean, x = treat, label = str_trim(.group)),
    color = "red",
    hjust = 1,
    position = position_nudge(x = -0.3)
  ) + 
  scale_fill_manual(name = "Treatment", values = treat_colors) +
  ylim(0, NA) + # force y-axis to start at 0
  ylab("Yield in t/ha") + # label y-axis
  xlab("Treatment") +      # label x-axis
  labs(caption = "Black dots represent raw data
       Red dots and error bars represent adjusted mean with 95% confidence limits per treatment
       Means followed by a common letter are not significantly different according to the Tukey-test") +
  theme_classic() + # clearer plot format 
  theme(plot.caption.position = "plot")
