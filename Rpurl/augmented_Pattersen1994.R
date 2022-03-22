# packages
pacman::p_load(tidyverse,        # data import and handling
               conflicted,       # handling function conflicts
               lme4, lmerTest,   # linear mixed model 
               emmeans, multcomp, multcompView, # mean comparisons
               ggplot2, desplot) # plots 

# conflicts: identical function names from different packages
conflict_prefer("lmer", "lmerTest")

# data (import via URL)
dataURL <- "https://raw.githubusercontent.com/SchmidtPaul/DSFAIR/master/data/Pattersen1994.csv"
dat <- read_csv(dataURL)

dat

dat <- dat %>% 
  mutate_at(vars(gen, block), as.factor)

# create a color-generating function
get_shades_of_blue <- colorRampPalette(colors=c("skyblue", "royalblue4"))
blue_30  <- get_shades_of_blue(30) # 30 shades of blue
orange_3 <- c("darkorange", "darkorange2", "darkorange3") # 3 shades of orange
blue_30_and_orange_3 <- c(blue_30, orange_3) # combine into single vector   

# name this vector of colors with genotype names
names(blue_30_and_orange_3) <- dat %>% 
  pull(gen) %>% as.character() %>% sort %>% unique

desplot(data = dat, flip = TRUE,
        form = gen ~ col + row, # fill color per genotype, headers per replicate
        col.regions = blue_30_and_orange_3,  # custom colors
        text = gen, cex = 1, shorten = "no", # show genotype names per plot
        out1 = block,                        # lines between complete blocks/replicates
        main = "Field layout", show.key = F) # formatting

dat %>% 
  group_by(gen) %>% 
  summarize(mean    = mean(yield),
            std.dev = sd(yield)) %>% 
  arrange(std.dev, desc(mean)) %>% # sort
  print(n=Inf) # print full table

dat %>% 
  group_by(block) %>% 
  summarize(mean    = mean(yield),
            std.dev = sd(yield))  %>% 
  arrange(desc(mean)) %>% # sort
  print(n=Inf) # print full table

ggplot(data = dat, 
       aes(y = yield,
           x = gen,
           color = gen,
           shape = block)) +
  geom_point(size = 2) + # scatter plot with larger dots
  ylim(0, NA) + # force y-axis to start at 0
  scale_color_manual(values = blue_30_and_orange_3) + # custom colors
  guides(color = "none") + # turn off legend for colors
  theme_classic() + # clearer plot format
  theme(legend.position = "top") # legend on top

# blocks as fixed (linear model)
mod.fb <- lm(yield ~ gen + block,
             data = dat)

mod.fb %>%
  emmeans(pairwise ~ "gen",
          adjust = "tukey") %>%
  pluck("contrasts") %>% # extract diffs
  as_tibble %>% # format to table
  pull("SE") %>% # extract s.e.d. column
  mean() # get arithmetic mean

# blocks as random (linear mixed model)
mod.rb <- lmer(yield ~ gen + (1 | block),
               data = dat)

mod.rb %>%
  emmeans(pairwise ~ "gen",
          adjust = "tukey",
          lmer.df = "kenward-roger") %>%
  pluck("contrasts") %>% # extract diffs
  as_tibble %>% # format to table
  pull("SE") %>% # extract s.e.d. column
  mean() # get arithmetic mean

mod.fb %>% anova()

mean_comparisons <- mod.fb %>%
  emmeans(pairwise ~ "gen",
          adjust = "tukey") %>%
  pluck("emmeans") %>%
  cld(details = TRUE, Letters = letters) # add letter display

# If cld() does not work, try CLD() instead.
# Add 'adjust="none"' to the emmeans() and cld() statement
# in order to obtain t-test instead of Tukey!

mean_comparisons$emmeans # adjusted genotype means

# genotypes ordered by mean yield
gen_order_emmean <- mean_comparisons$emmeans %>% 
  arrange(emmean) %>% 
  pull(gen) %>% 
  as.character()

# assign this order to emmeans object
mean_comparisons$emmeans <- mean_comparisons$emmeans %>% 
  mutate(gen = fct_relevel(gen, gen_order_emmean))

# assign this order to dat object
dat <- dat %>% 
  mutate(gen = fct_relevel(gen, gen_order_emmean))

ggplot() +
  # blue/orange dots representing the raw data
  geom_point(
    data = dat,
    aes(y = yield, x = gen, color = gen)
  ) +
  scale_color_manual(values = blue_30_and_orange_3) + # custom colors
  guides(color = "none") + # turn off legend for colors
  # red dots representing the adjusted means
  geom_point(
    data = mean_comparisons$emmeans,
    aes(y = emmean, x = gen),
    color = "red",
    position = position_nudge(x = 0.2)
  ) +
  # red error bars representing the confidence limits of the adjusted means
  geom_errorbar(
    data = mean_comparisons$emmeans,
    aes(ymin = lower.CL, ymax = upper.CL, x = gen),
    color = "red",
    width = 0.1,
    position = position_nudge(x = 0.2)
  ) +
  # red letters 
  geom_text(
    data = mean_comparisons$emmeans,
    aes(y = lower.CL, x = gen, label = .group),
    color = "red",
    angle = 90,
    hjust = 1,
    position = position_nudge(y = - 25)
  ) + 
  ylim(0, NA) + # force y-axis to start at 0
  ylab("Yield in t/ha") + # label y-axis
  xlab("Genotype") +      # label x-axis
  labs(caption = "Blue/orange dots represent raw data
       Red dots and error bars represent adjusted mean with 95% confidence limits per genotype
       Means followed by a common letter are not significantly different according to the Tukey-test") +
  theme_classic() # clearer plot format 

# data (import via URL)
dataURL <- "https://raw.githubusercontent.com/SchmidtPaul/DSFAIR/master/data/PiephoAugmentedLattice.csv"
ex1dat <- read_csv(dataURL)

desplot(data = ex1dat, flip = TRUE,
        form = block ~ col + row | rep, # fill color per block, headers per replicate
        text = geno, cex = 0.75, shorten = "no", # show genotype names per plot
        col  = genoCheck, # different color for check genotypes
        out1 = rep,   out1.gpar = list(col = "black"), # lines between reps
        out2 = block, out2.gpar = list(col = "darkgrey"), # lines between blocks
        main = "Field layout", show.key = F) # formatting

## ex1dat <- ex1dat %>%
##   mutate_at(vars(rep:genoCheck), as.factor)
## 
## # Draw a plot with yield per genotype
## ggplot(data = ex1dat,
##        aes(y = yield,
##            x = geno,
##            color = genoCheck)) +
##   geom_point(size = 2) + # scatter plot with larger dots
##   ylim(0, NA) + # force y-axis to start at 0
##   guides(color = "none") + # turn off legend for colors
##   theme_classic() + # clearer plot format
##   theme(axis.text.x = element_text(angle=90, hjust=1, vjust=0.5), # rotate axis label
##         panel.grid.major.x = element_line()) # add vertikal grid lines
## 
## # Set up two models: One with blocks as fixed and one with blocks as random.
## mod.fb <- lm(yield ~ geno + rep + block,
##              data = ex1dat)
## 
## mod.rb <- lmer(yield ~ geno + rep + (1 | block),
##                data = ex1dat)
## 
## # Compare their average s.e.d. between genotype means.
## # Choose the model with the smaller value.
## 
## mod.fb %>%
##   emmeans(pairwise ~ "geno",
##           adjust = "tukey") %>%
##   pluck("contrasts") %>% # extract diffs
##   as_tibble %>% # format to table
##   pluck("SE") %>% # extract s.e.d. column
##   mean() # 37.8356
## 
## mod.rb %>%
##   emmeans(pairwise ~ "geno",
##           adjust = "tukey",
##           lmer.df = "kenward-roger") %>%
##   pluck("contrasts") %>% # extract diffs
##   as_tibble %>% # format to table
##   pluck("SE") %>% # extract s.e.d. column
##   mean() # 36.84507
## 
## # Compute an ANOVA for the chose model.
## mod.rb %>% anova(ddf = "Kenward-Roger")
## 
## # Perform multiple (mean) comparisons based on the chosen model.
## mod.rb %>%
##   emmeans(pairwise ~ "geno",
##           adjust = "tukey",
##           lmer.df = "kenward-roger") %>%
##   pluck("emmeans") %>%
##   cld(details = TRUE, Letters = letters) # add letter display
