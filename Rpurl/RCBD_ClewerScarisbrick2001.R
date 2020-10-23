# packages
pacman::p_load(readr, tidyverse,     # data import and handling
               emmeans, multcomp,    # mean comparisons
               ggplot2, desplot,     # plots
               report, equatiomatic) # automated analysis summaries

# data (import via URL)
dataURL <- "https://raw.githubusercontent.com/SchmidtPaul/DSFAIR/master/data/Clewer%26Scarisbrick2001.csv"
dat <- read_csv(dataURL)

dat

dat <- dat %>% 
  mutate_at(vars(block, cultivar), as.factor)

desplot(data = dat,
  form = cultivar ~ col + row,        # fill color per cultivar
  out1 = block,                       # bold lines between blocks
  text = yield, cex = 1, shorten = F, # show yield for each plot
  main = "Field layout", show.key = F) # formatting

dat %>% 
  dplyr::select(block, cultivar, yield) %>% 
  pivot_wider(names_from = cultivar, values_from = yield)

dat %>% 
  group_by(cultivar) %>% 
  summarize(mean    = mean(yield),
            std.dev = sd(yield))

dat %>% 
  group_by(block) %>% 
  summarize(mean    = mean(yield),
            std.dev = sd(yield))

ggplot(data = dat,
       aes(y = yield, x = cultivar, color = block)) +
  geom_point() +  # scatter plot
  ylim(0, NA) +   # force y-axis to start at 0
  theme_classic() # clearer plot format 

mod <- lm(yield ~ cultivar + block, data = dat)

mod %>% anova()

mean_comparisons <- mod %>% 
  emmeans(pairwise ~ "cultivar", adjust="tukey") %>% # adjust="none" for t-test
  pluck("emmeans") %>% 
  cld(details=TRUE, Letters=letters) # add letter display

mean_comparisons$emmeans # adjusted cultivar means

mean_comparisons$comparisons # differences between adjusted cultivar means 

ggplot() +
  # black dots representing the raw data
  geom_point(
    data = dat,
    aes(y = yield, x = cultivar)
  ) +
  # red dots representing the adjusted means
  geom_point(
    data = mean_comparisons$emmeans,
    aes(y = emmean, x = cultivar),
    color = "red",
    position = position_nudge(x = 0.1)
  ) +
  # red error bars representing the confidence limits of the adjusted means
  geom_errorbar(
    data = mean_comparisons$emmeans,
    aes(ymin = lower.CL, ymax = upper.CL, x = cultivar),
    color = "red",
    width = 0.1,
    position = position_nudge(x = 0.1)
  ) +
  # red letters 
  geom_text(
    data = mean_comparisons$emmeans,
    aes(y = emmean, x = cultivar, label = .group),
    color = "red",
    position = position_nudge(x = 0.2)
  ) + 
  ylim(0, NA) + # force y-axis to start at 0
  ylab("Yield in t/ha") + # label y-axis
  xlab("Cultivar") +      # label x-axis
  labs(caption = "Black dots represent raw data
       Red dots and error bars represent adjusted mean with 95% confidence limits per cultivar
       Means followed by a common letter are not significantly different according to the Tukey-test") +
  theme_classic() # clearer plot format 

dat %>% 
  dplyr::select(-row, -col) %>% 
  report() %>% text_short()

mod %>% extract_eq()

mod %>% report() %>% text_short()

mod %>% 
  anova %>% 
  report() %>% text_short()

# data (import via URL)
dataURL <- "https://raw.githubusercontent.com/SchmidtPaul/DSFAIR/master/data/Gomez%26Gomez1984b.csv"
ex1dat <- read_csv(dataURL)

## # Exercise 1 of Example 2
## pacman::p_load(readr, tidyverse,     # data import and handling
##                emmeans, multcomp,    # mean comparisons
##                ggplot2, desplot,     # plots
##                report, equatiomatic) # automated analysis summaries
## 
## # format
## ex1dat <- ex1dat %>%
##   mutate_at(vars(density), as.factor)
## 
## # Create a field layout with desplot()
## desplot(data = ex1dat,
##         form = density ~ Row + Col,
##         out1 = block)
## 
## # Draw a plot with moisture values per soil
## ggplot(data = ex1dat,
##        aes(y = yield, x = density, color = block)) +
##   geom_point() +
##   ylim(0, NA) +
##   theme_classic()
## 
## # Compute an ANOVA
## # modelling
## mod <- lm(yield ~ density + block, data = ex1dat)
## 
## # anova
## mod %>% anova() # treatment is significant!
## 
## # Perform multiple (mean) comparisons
## mean_comparisons <- mod %>%
##   emmeans(pairwise ~ "density", adjust="tukey") %>% # adjust="none" for t-test
##   pluck("emmeans") %>%
##   cld(details=TRUE, Letters=letters)
