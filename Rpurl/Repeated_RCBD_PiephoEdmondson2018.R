# packages
pacman::p_load(conflicted, # handle conflicting functions
               agriTutorial, tidyverse,  # data import and handling
               nlme, glmmTMB, # linear mixed modelling
               mixedup, AICcmodavg, car, # linear mixed model processing
               emmeans, multcomp, # mean comparisons
               ggplot2, gganimate, gifski)  # (animated) plots 

conflict_prefer("select", "dplyr") # set select() from dplyr as default
conflict_prefer("filter", "dplyr") # set filter() from dplyr as default

# data (import via URL)
dat <- agriTutorial::sorghum %>% # data from agriTutorial package
  rename(block = Replicate, 
         weekF = factweek,  # week as factor
         weekN = varweek,   # week as numeric/integer
         plot  = factplot) %>% 
  mutate(variety = paste0("var", variety),    # variety id
         block   = paste0("block", block),    # block id
         weekF   = paste0("week", weekF),     # week id
         plot    = paste0("plot", plot),      # plot id
         unit    = paste0("obs", 1:n() )) %>% # obsevation id
  mutate_at(vars(variety:plot, unit), as.factor) %>% 
  as_tibble()

dat

dat %>% 
  group_by(variety) %>% 
  summarize(mean    = mean(y, na.rm=TRUE),
            std.dev = sd(y, na.rm=TRUE)) %>% 
  arrange(desc(mean)) %>% # sort
  print(n=Inf) # print full table

dat %>% 
  group_by(weekF, variety) %>% 
  summarize(mean = mean(y, na.rm=TRUE)) %>% 
  pivot_wider(names_from = weekF, values_from = mean)  

var_colors <- c("#8cb369", "#f4a259", "#5b8e7d", "#bc4b51")
names(var_colors) <- dat$variety %>% levels()

gganimate_plot <- ggplot(
  data = dat, aes(y = y, x = weekF,
                  group = variety,
                  color = variety)) +
  geom_boxplot(outlier.shape = NA) +
  geom_point(alpha = 0.5, size = 3) +
  scale_y_continuous(
    name = "Leaf area index",
    limits = c(0, 6.5),
    expand = c(0, 0),
    breaks = c(0:6)
  ) +
  scale_color_manual(values = var_colors) +
  theme_bw() +
  theme(legend.position = "bottom", 
        axis.title.x = element_blank()) +
  transition_time(weekN) +
  shadow_mark(exclude_layer = 2) 

animate(gganimate_plot, renderer = gifski_renderer()) # render gif

dat.wk1 <- dat %>% filter(weekF == "week1") # subset data from first week only

mod.wk1 <- lm(formula = y ~ variety + block,
              data = dat.wk1)

mod.iid <- glmmTMB(formula = y ~ weekF * (variety + block) 
                        + (1 | unit),      # add random unit term to mimic error variance
                        dispformula = ~ 0, # fix original error variance to 0
                        REML = TRUE,       # needs to be stated since default = ML
                        data = dat) 

# Extract variance component estimates
# alternative: mod.iid %>% broom.mixed::tidy(effects = "ran_pars", scales = "vcov")
mod.iid %>% mixedup::extract_vc(ci_scale = "var") 

mod.iid.nlme <- gls(model = y ~ weekF * (block + variety), 
                    correlation = NULL, # default, i.e. homoscedastic, independent errors
                    data = dat)

# Extract variance component estimates
tibble(varstruct = "iid") %>% 
  mutate(sigma    = mod.iid.nlme$sigma) %>% 
  mutate(Variance = sigma^2)

mod.AR1 <- glmmTMB(formula = y ~ weekF * (variety + block) +
                     ar1(weekF + 0 | plot), # ar1 structure as random term to mimic error var
                   dispformula = ~ 0, # fix original error variance to 0
                   REML = TRUE,       # needs to be stated since default = ML
                   data = dat) 

# Extract variance component estimates
# alternative: mod.ar1 %>% broom.mixed::tidy(effects = "ran_pars", scales = "vcov")
mod.AR1 %>% extract_vc(ci_scale = "var", show_cor = TRUE) 

mod.AR1.nlme <- gls(model = y ~ weekF * (block + variety),
                    correlation = corAR1(form = ~ weekN | plot),
                    data = dat)

# Extract variance component estimates
tibble(varstruct = "ar(1)") %>%
  mutate(sigma    = mod.AR1.nlme$sigma,
         rho      = coef(mod.AR1.nlme$modelStruct$corStruct, unconstrained = FALSE)) %>%
  mutate(Variance = sigma^2,
         Corr1wk  = rho,
         Corr2wks = rho^2,
         Corr3wks = rho^3,
         Corr4wks = rho^4)

mod.AR1.nlme.V2 <- gls(model = y ~ weekF * (variety + block),
                       correlation = corExp(form = ~ weekN | plot),
                       data = dat)

tibble(varstruct = "ar(1)") %>%
  mutate(sigma    = mod.AR1.nlme.V2$sigma,
         rho      = exp(-1/coef(mod.AR1.nlme.V2$modelStruct$corStruct, 
                                unconstrained = FALSE))) %>%
  mutate(Variance = sigma^2,
         Corr1wk  = rho,
         Corr2wks = rho^2,
         Corr3wks = rho^3,
         Corr4wks = rho^4)

## mod.AR1nugget <- glmmTMB(formula = y ~ weekF * (variety + block) +
##                            ar1(weekF + 0 | plot), # ar1 structure as random term to mimic error var
##                          # dispformula = ~ 0, # error variance allowed = nugget!
##                          REML = TRUE,       # needs to be stated since default = ML
##                          data = dat)
## 
## # show variance components
## # alternative: mod.AR1nugget %>% broom.mixed::tidy(effects = "ran_pars", scales = "vcov")
## mod.AR1nugget %>% extract_vc(ci_scale = "var", show_cor = TRUE)
## 
## # We can see that the we get $\sigma^2_{plot} =$ `0.019`, an additional residual/nugget variance  $\sigma^2_{N} =$ `0.004` and a $\rho =$ of `0.908`.

mod.AR1nugget.nlme <- gls(model = y ~ weekF * (block + variety),
                          correlation = corExp(form = ~ weekN | plot, nugget = TRUE),
                          data = dat)

tibble(varstruct = "ar(1) + nugget") %>%
  mutate(sigma    = mod.AR1nugget.nlme$sigma,
         nugget   = coef(mod.AR1nugget.nlme$modelStruct$corStruct, 
                         unconstrained = FALSE)[2],
         rho      = (1-coef(mod.AR1nugget.nlme$modelStruct$corStruct, 
                            unconstrained = FALSE)[2])*
                    exp(-1/coef(mod.AR1nugget.nlme$modelStruct$corStruct, 
                                unconstrained = FALSE)[1])) %>%
  mutate(Variance = sigma^2,
         Corr1wk  = rho,
         Corr2wks = rho^2,
         Corr3wks = rho^3,
         Corr4wks = rho^4)

mod.hCS <- glmmTMB(formula = y ~ weekF * (variety + block) +
                     cs(weekF + 0 | plot), # hcs structure as random term to mimic error var
                   dispformula = ~ 0, # fix original error variance to 0
                   REML = TRUE,       # needs to be stated since default = ML
                   data = dat) 

# show variance components
# alternative: mod.hCS %>% broom.mixed::tidy(effects = "ran_pars", scales = "vcov")
mod.hCS %>% extract_vc(ci_scale = "var", show_cor = TRUE) 

mod.CS.nlme <- gls(y ~ weekF * (block + variety),
                   corr = corCompSymm(form = ~ weekN | plot),
                   data = dat)

tibble(varstruct = "cs") %>%
  mutate(sigma    = mod.CS.nlme$sigma,
         rho      = coef(mod.CS.nlme$modelStruct$corStruct, unconstrained = FALSE)) %>%
  mutate(Variance = sigma^2,
         Corr1wk  = rho,
         Corr2wks = rho,
         Corr3wks = rho,
         Corr4wks = rho)

mod.Toep <- glmmTMB(formula = y ~ weekF * (variety + block) +
                     toep(weekF + 0 | plot), # teop structure as random term to mimic err var
                   dispformula = ~ 0, # fix original error variance to 0
                   REML = TRUE,       # needs to be stated since default = ML
                   data = dat) 

# show variance components
# alternative: mod.Toep %>% broom.mixed::tidy(effects = "ran_pars", scales = "vcov")
mod.Toep %>% extract_vc(ci_scale = "var", show_cor = TRUE) 

mod.UN <- glmmTMB(formula = y ~ weekF * (variety + block) +
                     us(weekF + 0 | plot), # us structure as random term to mimic error var
                   dispformula = ~ 0, # fix original error variance to 0
                   REML = TRUE,       # needs to be stated since default = ML
                   data = dat) 

# show variance components
# alternative: mod.UN %>% broom.mixed::tidy(effects = "ran_pars", scales = "vcov")
mod.UN %>% extract_vc(ci_scale = "var", show_cor = TRUE) 

mod.UN.nlme <- gls(y ~ weekF * (block + variety), 
                   corr = corSymm(form = ~ 1 | plot), 
                   weights = varIdent(form = ~ 1|weekF), 
                   data = dat)

# Extract variance component estimates: variances
mod.UN.nlme$modelStruct$varStruct %>%
  coef(unconstrained = FALSE, allCoef = TRUE) %>% 
  enframe(name = "grp", value = "varStruct") %>%
  mutate(sigma         = mod.UN.nlme$sigma) %>%
  mutate(StandardError = sigma * varStruct) %>%
  mutate(Variance      = StandardError ^ 2)

# Extract variance component estimates: correlations
mod.UN.nlme$modelStruct$corStruct

AICcmodavg::aictab(
  cand.set = list(mod.iid, mod.hCS, mod.AR1, mod.Toep, mod.UN), 
  modnames = c("iid", "hCS", "AR1", "Toeplitz", "UN"),
  second.ord = FALSE) # get AIC instead of AICc

AICcmodavg::aictab(
  cand.set = list(mod.iid.nlme, mod.CS.nlme, mod.AR1.nlme, mod.AR1nugget.nlme, mod.UN.nlme), 
  modnames = c("iid", "CS", "AR1", "AR1 + nugget", "UN"),
  second.ord = FALSE) # get AIC instead of AICc

ggplot(data = dat, 
       aes(y = y, x = weekF,
           group = variety,
           color = variety)) +
  geom_point(alpha = 0.75, size = 3) +
  stat_summary(fun=mean, geom="line") + # lines between means
  scale_y_continuous(
    name = "Leaf area index",
    limits = c(0, 6.5),
    expand = c(0, 0),
    breaks = c(0:6)) +
  scale_color_manual(values = var_colors) +
  theme_bw() +
  theme(legend.position = "bottom", 
        axis.title.x = element_blank())

mod.AR1 %>% 
ggeffects::ggemmeans(terms = c("weekF", "variety"),
                     ci.lvl = 0.95)

glmmTMB(formula = y ~ 0 + 
          variety + variety:weekN +
          weekF*block +
          ar1(weekF + 0 | plot), # ar1 structure as random term to mimic error var
        dispformula = ~ 0, # fix original error variance to 0
        REML = TRUE,       # needs to be stated since default = ML
        data = dat) %>% 
  ggeffects::ggemmeans(terms = c("weekN", "variety"),
                       ci.lvl = 0.95) %>% 
  ggplot(., aes(x=x, 
                y=predicted)) +
  scale_color_manual(values = var_colors) +
  scale_fill_manual(values = var_colors) +
  theme_bw() +
  theme(legend.position = "bottom", 
        axis.title.x = element_blank()) +
  scale_y_continuous(
    name = "Leaf area index",
    limits = c(0, 6.5),
    expand = c(0, 0),
    breaks = c(0:6)) +
  geom_line(aes(colour = group), 
            size = 1.5) +
  geom_ribbon(aes(ymin = conf.low, 
                  ymax = conf.high, 
                  fill = group), 
              alpha = 0.2)

