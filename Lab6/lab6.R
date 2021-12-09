#!/usr/bin/env Rscript

library(dplyr)
library(drc)
library(ggplot2)
# Zad 1.1
df = readxl::read_excel(file.choose())
head(df)
plot(df)
model = drm(v ~ S, data = df, fct = MM.2())
summary(model)
ggplot(df, aes(S, v)) + geom_point() + geom_smooth(aes(S, predict(model)), color = "red", cex = .5)

# Zad 1.2
model = nls(v ~ (S * a) / (S + b), data = df, start = list(a = 0.00542 , b = 0.43980))
summary(model)

# Zad 2
df = read.table(file.choose(), header = T)
df = df %>% tibble() %>% dplyr::select(-Lp)
model = nls(Plon ~ a * (Tydzien^b)* exp(-c * Tydzien), data = df, start = list(a = 1, b = 2, c = 0.2))
ggplot(data = df, aes(Tydzien, Plon)) + geom_point() + geom_smooth(aes(Tydzien, predict(model)), color = "blue")

# Zad 3
df1 = readr::read_table("dataset1.txt")
df2 = readr::read_table("dataset2.txt")

# Dataset1
p = df1 %>% ggplot() + geom_point(aes(time, response, color = time))
p
model = drm(response ~ time, data = df1, fct = logistic())
p + geom_smooth(aes(time, predict(model)), color="red")
# Dataset2
model = drm(response ~ time, data = df1, fct = )
model.comp = mselect(model, list(logistic(), L.3(), L.4(), L.5(), l2(), l3(), ), linreg = T)
p = df2 %>% ggplot() + geom_point(aes(time, response, color = time))
p
