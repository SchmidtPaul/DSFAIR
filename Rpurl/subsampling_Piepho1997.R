# packages
pacman::p_load(readr, tidyverse, forcats, # data import and handling
               lme4, lmerTest,            # linear mixed model 
               emmeans, multcomp,         # mean comparisons
               ggplot2, desplot, see)     # plots

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
  scale_fill_manual(values = treat_colors) +
  ylim(0, NA) +
  theme_modern()

mod <- lmer(TKW ~ treat + block + (1|block:treat),
            data=dat)

mod %>% 
  VarCorr() %>% 
  as.data.frame() %>% 
  dplyr::select(grp, vcov)

mod %>% anova(ddf="Kenward-Roger")

mean_comparisons <- mod %>% 
  emmeans(pairwise ~ "treat",
          adjust  = "tukey",
          lmer.df = "kenward-roger") %>% 
  pluck("emmeans") %>%
  cld(details = TRUE, Letters = letters) # add letter display

mean_comparisons$emmeans
