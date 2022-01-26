#!/usr/bin/env Rscript
library(dplyr)
library(tibble)
library(tidyr)
library(magrittr) # For other pipeline operators

# 1
df = iris %>% tibble()

# Sepal
df %>% group_by(Species) %>% rstatix::shapiro_test(Sepal.Length, Sepal.Width) # Passed
analysis.sepal = manova(cbind(Sepal.Length, Sepal.Width) ~ Species, data = df)

summary(analysis.sepal, test = "Pillai")
summary(analysis.sepal, test = "Wilks")
summary(analysis.sepal, test = "Hotelling-Lawley")
summary(analysis.sepal, test = "Roy")

# Petal
df %>% rstatix::mshapiro_test(Petal.Length, Petal.Width) # Didn't passed
analysis.petal = manova(cbind(Petal.Length, Petal.Width) ~ Species, data = df)

summary(analysis.petal, test = "Pillai")
summary(analysis.petal, test = "Wilks")
summary(analysis.petal, test = "Hotelling-Lawley")
summary(analysis.petal, test = "Roy")

# Together
rstatix::mshapiro_test(df %>% select(-Species)) # Didn't passed
analysis.together = manova(cbind(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width) ~ Species, data = df)

summary(analysis.together, test = "Pillai")
summary(analysis.together, test = "Wilks")
summary(analysis.together, test = "Hotelling-Lawley")
summary(analysis.together, test = "Roy")

# 2
df = readxl::read_excel("metals.xlsx")
res.shapiro = df %>% group_by(extractants, metals) %>% rstatix::shapiro_test(C1, C2, C3, C4) # Passed

metals.res = manova(cbind(C1, C2, C3, C4) ~ extractants + metals, data = df)
summary(metals.res, test = "Pillai")
summary(metals.res, test = "Wilks")
summary(metals.res, test = "Hotelling-Lawley")
summary(metals.res, test = "Roy")

# Drzewa
df = readxl::read_excel("jaja-tree.xlsx")
tree = rpart::rpart(zaplodnienie ~ FA + HA + HT + ST + EST, data = df, method = "anova")
rpart.plot::rpart.plot(tree, box.palette = "Greens")

tree = rpart::rpart(niewyklute ~ FA + HA + HT + ST + EST, data = df, method = "anova")
rpart.plot::rpart.plot(tree, box.palette = "Greens")

