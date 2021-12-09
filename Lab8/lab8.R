#!/usr/bin/env Rscript
library(readr)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)

# 1
curve(1 / (1 + exp(-x)),-10, 10)

# 2
df = read_excel(file.choose())

ggplot(df) + geom_bar(aes(palenie, nadcisnienie), stat = "identity", fill = "red")
ggplot(df) + geom_bar(aes(as.factor(wiek), nadcisnienie),
                      stat = "identity",
                      fill = "blue") + labs(x = "wiek", y = "nadcisnienie")
ggplot(df) + geom_point(aes(wiek, nadcisnienie))
ggplot(df) + geom_point(aes(palenie, nadcisnienie))


model = glm(
    as.factor(nadcisnienie) ~ as.factor(palenie) + wiek,
    data = df,
    family = binomial(logit)
)
df$nadcisnienie
ifelse(predict(model) > 0.5, 1, 0) %>% as.vector
# Binomial = 21.478
# Gaussian = 24.312
# Poisson = 29.585

# 3
# A
AIC(lm(mpg ~ disp + hp, mtcars)) # 168.6186
model = glm(mpg ~ disp + hp, mtcars, family = gaussian())
summary(model) # 168.62

# B
glm(am ~ disp + hp, mtcars, family = binomial())$aic
glm(am ~ disp + hp, mtcars, family = poisson())$aic
# binomial AIC 22.71293
# poisson AIC 42.52571

# 4
df = read_table(file.choose())
df = df %>% select(-Lp)
ggplot(df) + geom_smooth(aes(Days, Students))
glm(Students ~ Days, df, family = binomial())
glm(Students ~ Days, df, family = gaussian()) # AIC 421.3
glm(Students ~ Days, df, family = poisson()) # AIC 393.1

# 5
ggplot(PlantGrowth) + geom_boxplot(aes(x = group, y = weight, fill = group))
df = PlantGrowth %>% tibble()
df = df %>% mutate(weight = cut(weight, breaks = 2, labels = c(0, 1)))
glm(weight ~ group, df, family = binomial())

# 6
df = read_table("Lp	ck	ha	ok
1	20	2	88
2	60	13	26
3	100	30	8
4	140	30	5
5	180	21	0
6	220	19	1
7	260	18	1
8	300	13	1
9	340	19	1
10	380	15	0
11	420	7	0
12	460	8	0
") %>% select(-Lp)
# A
df = df %>% mutate(prob_ha = ha / (ha + ok))
ggplot(df) + geom_point(aes(ck, prob_ha, color = prob_ha))
# B
model = glm(cbind(ha, ok) ~ ck, df, family = "binomial") # AIC: 62.33
# C
model
summary(model)
# D
model.sq = glm(cbind(ha, ok) ~ ck + I(ck^2) , df, family = "binomial") # AIC: 42.82
# E
model.sq
summary(model.sq)
df$binomial_fit = model$fitted.values
df$binomial_sq_fit = model.sq$fitted.values
df.pivoted = pivot_longer(df, cols = binomial_fit:binomial_sq_fit, names_to = "method")
# F
# Lepszy model to model z AIC 42.82
# G
ggplot(df.pivoted) + geom_smooth(aes(x = ck, y = value, color = method), se = F) + geom_smooth(aes(x = ck, y = ha/(ha + ok), color = "original"), se = F)
