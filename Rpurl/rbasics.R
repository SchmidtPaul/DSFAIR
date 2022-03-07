2+3
2 *   6

sqrt(9)

pi
letters

x = 3
x
x = 20
x
a_long_variable_name <- 2 + 4
a_long_variable_name
x + a_long_variable_name
mytext <- "This is my text"
mytext

typeof(x)
typeof(mytext)

length(letters)
str(letters)

str(pi)
length(pi)

mynumbers <- c(1, 4, 9, 12, 12, 12, 16)
mynumbers
mywords <- c("Hakuna", "Matata", "Simba")
mywords

sqrt(mynumbers)

mean(mynumbers)

seq(1, 10)

seq(10, 1)

seq(from = 1, to = 10, by = 1)

seq(1, 9, 2)
seq(from = 1, to = 9, by = 2)
seq(from = 1, by = 2, to = 9)
seq(1, 2, 9)

## pacman::p_load(
##   package_name_1,
##   package_name_2,
##   package_name_3
## )

PlantGrowth

df <- PlantGrowth
str(df)
summary(df)

df$weight
df$group

library(tibble)

tbl <- as_tibble(df)
tbl

plot(df$weight) # scatter plot of values in the order they appear
plot(df$group) # bar plot of frequency of each level
plot(x = df$group, y = df$weight) # boxplot for values of each level

library(dplyr) # make %>% available 

sort(round(sqrt(pull(filter(PlantGrowth, group == "ctrl"), weight)), 1))

a <- filter(PlantGrowth, group == "ctrl")
b <- pull(a, weight)
c <- sqrt(b)
d <- round(c, 1)
sort(d)

PlantGrowth %>% 
  filter(group == "ctrl") %>% 
  pull(weight) %>% 
  sqrt() %>% 
  round(1) %>% 
  sort()
