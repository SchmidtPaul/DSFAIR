# packages
pacman::p_load(tidyverse, # data import and handling
               conflicted, # handling function conflicts
               emmeans, multcomp, multcompView, # adjusted mean comparisons
               ggplot2, desplot) # plots

# conflicts between functions with the same name
conflict_prefer("filter", "dplyr") 
conflict_prefer("select", "dplyr")

# data (import via URL)
dataURL <- "https://raw.githubusercontent.com/SchmidtPaul/DSFAIR/master/data/Mead1993.csv"
dat <- read_csv(dataURL)

dat

dat <- dat %>% 
  mutate_at(vars(variety), as.factor)

desplot(data = dat, flip = TRUE,
        form = variety ~ col + row,              # fill color per variety
        text = variety, cex = 1, shorten = "no", # show variety names per plot
        main = "Field layout", show.key = F)     # formatting

dat %>% 
  group_by(variety) %>% 
  summarize(mean    = mean(yield),
            std.dev = sd(yield))

ggplot(data = dat,
       aes(y = yield, x = variety)) +
  geom_point() +  # scatter plot
  ylim(0, NA) +   # force y-axis to start at 0
  theme_classic() # clearer plot format 

mod <- lm(yield ~ variety, data = dat)

mod %>% anova()

mean_comparisons <- mod %>% 
  emmeans(specs = "variety") %>% # get adjusted means for varieties
  cld(adjust="tukey", Letters=letters) # add compact letter display

mean_comparisons

ggplot() +
  # black dots representing the raw data
  geom_point(
    data = dat,
    aes(y = yield, x = variety)
  ) +
  # red dots representing the adjusted means
  geom_point(
    data = mean_comparisons,
    aes(y = emmean, x = variety),
    color = "red",
    position = position_nudge(x = 0.1)
  ) +
  # red error bars representing the confidence limits of the adjusted means
  geom_errorbar(
    data = mean_comparisons,
    aes(ymin = lower.CL, ymax = upper.CL, x = variety),
    color = "red",
    width = 0.1,
    position = position_nudge(x = 0.1)
  ) +
  # red letters 
  geom_text(
    data = mean_comparisons,
    aes(y = emmean, x = variety, label = str_trim(.group)),
    color = "red",
    position = position_nudge(x = 0.2),
    hjust = 0
  ) + 
  ylim(0, NA) + # force y-axis to start at 0
  ylab("Yield in t/ha") + # label y-axis
  xlab("Variety") +      # label x-axis
  labs(caption = "Black dots represent raw data
       Red dots and error bars represent adjusted mean with 95% confidence limits per variety
       Means followed by a common letter are not significantly different according to the Tukey-test") +
  theme_classic() # clearer plot format 

# data (import via URL)
dataURL <- "https://raw.githubusercontent.com/SchmidtPaul/DSFAIR/master/data/Mead1993b.csv"
ex1dat <- read_csv(dataURL)

## # exercise 1
## 
## # packages
## pacman::p_load(readr, tidyverse, forcats, # data import and handling
##                emmeans, multcomp, multcompView, # mean comparisons
##                ggplot2, desplot,          # plots
##                report, equatiomatic)      # automated analysis summaries
## 
## # data (import via URL)
## dataURL <- "https://raw.githubusercontent.com/SchmidtPaul/DSFAIR/master/data/Mead1993b.csv"
## ex1dat <- read_csv(dataURL)
## 
## # in case of removing all moisture values larger than 12 run this:
## # ex1dat <- ex1dat %>%
## #   filter(moisture <= 12)
## 
## # format first!
## ex1dat <- ex1dat %>%
##   mutate_at(vars(soil), as.factor)
## 
## # How many samples per soil were taken?
## ex1dat %>%
##   count(soil)
## 
## # Which soil has the highest value for moisture?
## ex1dat %>%
##   group_by(soil) %>%
##   summarise(mean = mean(moisture)) %>%
##   arrange(desc(mean))
## 
## # Draw a plot with moisture values per soil
## ggplot(data = ex1dat,
##        aes(y = moisture, x = soil)) +
##   geom_point() +
##   ylim(0, NA) +
##   theme_classic()
## 
## # Compute an ANOVA
## mod <- lm(moisture ~ soil, data = ex1dat)
## 
## mod %>% anova() # significant!
## 
## # Perform multiple (mean) comparisons using the LSD test/t-test.
## mod %>%
##   emmeans(pairwise ~ "soil", adjust="none") %>% # adjust="none" for t-test
##   pluck("emmeans") %>%
##   cld(details=TRUE, Letters=letters)
