# packages
pacman::p_load(conflicted, # handle conflicting functions
               tidyverse,  # data import and handling
               glmmTMB,    # linear mixed model 
               broom.mixed, car, DHARMa, # linear mixed model evaluation
               emmeans, modelbased,      # mean comparisons
               desplot, scales, see)     # plots

# function conflicts
conflict_prefer("select", "dplyr", quiet = TRUE) # set select() from dplyr as default
conflict_prefer("filter", "dplyr", quiet = TRUE) # set filter() from dplyr as default

# data (import via URL)
dataURL <- "https://raw.githubusercontent.com/SchmidtPaul/DSFAIR/master/data/StreetFruit.csv"
dat <- read_csv(dataURL)

dat

dat <- dat %>% 
  mutate_at(vars(Yea:Blo, Plot), as.factor)

# Colors for Fru levels
Fru_color <- c(Apple = "#9bc53d",
               Banana = "#fde74c",
               Strawberry = "#f25f5c")

# Colors for Str levels
Str_color <- c(Control = "#C3B299",
               Street = "#313E50")

Yea_i <- "1990"
Loc_j <- "Paris"

dat %>% 
  filter(Yea == Yea_i & Loc == Loc_j) %>% 
  desplot(data = .,
    form = Fru ~ col + row, # fill color per Fru
    col.regions = Fru_color, # use predefined colors
    text = Str, cex = 1, shorten = "abb", # show genotype names per plot
    out1 = Blo,  out1.gpar = list(col = "black", lty = 1), # lines between blocks
    out2 = Plot, out2.gpar = list(col = "black", lty = 3), # lines betwen plots
    main = paste("Field layout:", Loc_j, Yea_i), # dynamic title
    show.key = TRUE, key.cex = 0.7) # formatting

dat %>% 
  filter(Yea == Yea_i & Loc == Loc_j) %>% 
  desplot(data = .,
    form = Blo ~ col + row, 
    col.regions = RColorBrewer::brewer.pal(12, "Paired")[c(1,3,5,7,9,11)],
    text = Str, cex = 1, shorten = "abb",
    col  = Fru, col.text = Fru_color,
    out1 = Blo,  out1.gpar = list(col = "black", lty = 1), 
    out2 = Plot, out2.gpar = list(col = "black", lty = 3), 
    show.key = TRUE, key.cex = 0.55,
    main = paste(Loc_j, "in", Yea_i))

dat %>% 
  filter(Yea == Yea_i & Loc == Loc_j) %>% 
  mutate(`Str:Blo` = interaction(Str, Blo)) %>% 
  desplot(data = .,
    form = `Str:Blo` ~ col + row, 
    col.regions = RColorBrewer::brewer.pal(12, "Paired"),
    text = Str, cex = 1, shorten = "abb",
    col  = Fru, col.text = Fru_color,
    out1 = Blo, out1.gpar = list(col = "black", lty = 1),
    out2 = Plot, out2.gpar = list(col = "black", lty = 3),
    show.key = TRUE, key.cex = 0.55,
    main = paste(Loc_j, "in", Yea_i))

dat %>% 
  filter(Yea == Yea_i & Loc == Loc_j) %>% 
  desplot(data = .,
    form = Str ~ col + row, 
    col.regions = Str_color,
    out1 = Plot, out1.gpar = list(col = "black", lty = 3),
    show.key = TRUE, key.cex = 0.55,
    main = paste(Loc_j, "in", Yea_i))

## glmmTMB(
##   Yie ~ Fru + Str:Fru + Str:Blo,
##   REML = TRUE,
##   data = dat %>% filter(Yea == Yea_i & Loc == Loc_j)
## )

dat %>% 
  filter(Yea == Yea_i) %>% 
  mutate(`Str:Blo` = interaction(Str, Blo)) %>% 
  desplot(data = .,
    form = Str ~ col + row|Loc,
    col.regions = Str_color,
    text = Fru, cex = 0.5, shorten = "abb",
    col  = Fru, col.text = Fru_color,
    out1 = `Str:Blo`, out1.gpar = list(col = "black", lty = 1),
    out2 = Plot, out2.gpar = list(col = "black", lty = 3),
    show.key = TRUE, key.cex = 0.55,
    main = paste("Both locations in", Yea_i))

dat <- dat %>% 
  mutate(Row = as.factor(row)) %>%    
  mutate(Blo = interaction(Row, Blo)) 

## glmmTMB(
##   Yie ~
##     Str + Fru + Str:Fru +                    # Treatment
##     Loc + (1 | Loc:Row) + (1 | Loc:Row:Blo), # Design
##   REML = TRUE,
##   data = dat %>% filter(Yea == Yea_i)
##   )

dat %>% 
  group_by(Fru, Str) %>% # or go all the way to group_by(Yea, Loc, Fru, Str)
  summarise(meanYield = mean(Yie),
            medYield  = median(Yie),
            maxYield  = max(Yie),
            minYield  = min(Yie),
            stddevYield = sd(Yie))

ggplot(data = dat,
       aes(y = Yie, x = Loc)) +    # y-axis: yield, x-axis: year
  facet_grid(cols = vars(Fru)) + # one facette per fruit
  geom_boxplot( # boxplot 
    aes(fill = Str),                 # box color
    position = position_dodge(0.66), # box distance
    width = 0.5                      # box width
  ) +
  geom_point( # scatter-plot
    aes(
      col = Fru,  # point color
      shape = Yea, # point shape
      group = Str  # point grouping
    ),
    position = position_dodge(0.66), # distance between groups
    size = 3,                        # dot size
    alpha = 0.75                     # dot transparency
  ) +
  stat_summary( # add mean as red point
    fun = mean,
    aes(group = Str), # per Str
    geom = "point", # as scatter point
    shape = 23,     # point shape
    size = 2,       # point size
    fill = "red",   # point color
    show.legend = FALSE, # hide legend for this
    position = position_dodge(0.66) # distance between groups
  ) +
  scale_y_continuous(#limits = c(0, NA),        # y-axis must start at 0
                     breaks = pretty_breaks(),
                     name = "Yield [ficticious unit]") +
  xlab("Location") + 
  scale_shape(name = "Year") +
  scale_fill_manual(values = Str_color, name = "Street") +
  scale_colour_manual(values = Fru_color, name = "Fruit") +
  theme_modern()

dat %>% 
  filter(Yea == Yea_i) %>% 
  desplot(data = .,
    form = Yie ~ col + row|Loc,
    text = Str, cex = 0.5, shorten = "abb",
    col  = Fru, col.text = Fru_color,
    out1 = Blo, out1.gpar = list(col = "black", lty = 1),
    # out2 = Plot, out2.gpar = list(col = "black", lty = 3),
    show.key = TRUE, key.cex = 0.55,
    main = paste("Absolute yield for both locations in", Yea_i))

dat %>%
  filter(Yea == Yea_i) %>% 
  group_by(Loc, Fru) %>% 
  mutate(
    meanYield        = mean(Yie),       
    ratioToMeanYield = Yie / mean(Yie)  
  ) %>% 
  ungroup() %>% 
    desplot(data = .,
    form = ratioToMeanYield ~ col + row|Loc,
    text = Str, cex = 0.5, shorten = "abb",
    col  = Fru, col.text = Fru_color,
    out1 = Blo, out1.gpar = list(col = "black", lty = 1),
    # out2 = Plot, out2.gpar = list(col = "black", lty = 3),
    show.key = TRUE, key.cex = 0.55,
    main = paste("Relative yield ratio to fruit-location-mean for both locations in", Yea_i))

mod1989 <- glmmTMB(
  Yie ~ 
    Str + Fru + Str:Fru +                    # Treatment
    Loc + (1 | Loc:Row) + (1 | Loc:Row:Blo), # Design
  REML = TRUE,
  data = dat %>% filter(Yea == "1989")
  )

DHARMa::plotResiduals(mod1989)

DHARMa::plotQQunif(mod1989)

mod1989 %>% 
  broom.mixed::tidy(effects = "ran_pars", scales = "vcov") %>% 
  select(group, estimate)

Anova(mod1989)

# emmeans:
mod1989 %>% 
  emmeans(pairwise ~ Str | Fru,
          adjust = "tukey",
          lmer.df = "kenward-roger") %>% 
  pluck("emmeans") %>%
  multcomp::cld(details = TRUE, Letters = letters)

# modelbased:
mod1989 %>%
  estimate_means(
    levels   = "Str",
    modulate = "Fru",
    lmer.df  = "kenward-roger",
    adjust   = "holm"
  )

mod1989 %>%
  estimate_contrasts(
    levels   = "Str",
    modulate = "Fru",
    lmer.df  = "kenward-roger",
    adjust   = "holm",
    standardize = FALSE
  )
