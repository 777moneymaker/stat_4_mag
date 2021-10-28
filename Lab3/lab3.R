#!/usr/bin/env Rscript

library(dplyr)
library(ggplot2)
library(tidyr)
# Iris
# 1
iris %>% select(Species, Sepal.Length)
# 2
iris %>% filter(Petal.Width > 2) %>% nrow()
# 3
iris %>% filter(Sepal.Width < 4 &
                    Species != "setosa") %>% 
    arrange(desc(Petal.Length))
# 4
iris %>% filter(Sepal.Length < 4.5) %>% 
    distinct(Species) %>% 
    nrow()
# 5
df = iris %>% 
    mutate(PLxPW = Petal.Length * Petal.Width)
df
# 6
df = iris %>% filter(Species == "virginica") %>% 
    select(Species, Petal.Length, Petal.Width)
p = ggplot(data = df) + geom_point(aes(Petal.Length, Petal.Width))
p
# 7
iris %>% group_by(Species) %>% summarise(N = n())
# 8
df = iris %>% 
    select(-Sepal.Length,-Sepal.Width) %>% 
    group_by(Species) %>%
    summarise(across(starts_with("Petal"), 
                     mean, 
                     .names = "{.col}.Mean"),
              surf = Petal.Length.Mean * Petal.Width.Mean) %>%
    filter(Species == "virginica" | surf != 13)


# Wina
# 1
df = read.csv(file.choose())
df = df %>% 
    as_tibble() %>% 
    select(-X)
ncol(df)
nrow(df)
# Dane mieszane - numeryczne, jakościowe porządkowe i nominalne
# 2
df %>% filter(points >= 94 & price < 25)
# 3
df %>% slice_sample(prop = .01)
# 4
df %>% slice_max(points, n = 3, with_ties = F)
# 5
df %>% slice_min(price, n = 100, with_ties = F)
# 6
df %>% select(title, points) %>% 
    arrange(desc(points))
# 7
df %>% select(title, country, province:region_2)
# 8
df %>% rename(score = points)
# 9
df %>% select(title, price) %>% 
    mutate(price_pln = price * 3.94)
# 10
df %>% summarise(mean_price = mean(price, na.rm = T))
df %>% summarise(price_quant = quantile(price, c(0, .1, .25, .5, .75, .9, 1), na.rm = T),
                 prob = c(0, .1, .25, .5, .75, .9, 1))
# 11
df %>% summarise(median_price = median(price, na.rm = T))
# 12
props_df = df %>% mutate(proportion = points / price) %>% 
    select(title, proportion, price, points) %>% 
    arrange(desc(proportion))
props_df
# 13
props_df %>% filter(points >= 90)
# 14
df %>% group_by(country) %>% 
    summarize(mean_price = mean(price, na.rm = T)) %>%
    slice_min(mean_price, n = 1)
# 15
df %>% group_by(country) %>% 
    mutate(mean_price = mean(price, na.rm = T), n_in_country = n()) %>% 
    select(title, price, mean_price:n_in_country)


df %>% group_by(country) %>% 
    mutate(mean_price = mean(price, na.rm = T), msqr = mean_price ^ 2, mquadr = msqr * mean_price) %>% 
    select(title, price, mean_price:mquadr)
