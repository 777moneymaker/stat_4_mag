library(dplyr)
library(tidyr)
library(readr)
library(readxl)
library(lavaanPlot)
library(lavaan)

df = readxl::read_excel("kukurydza.xlsx")
# Zadanie 1
anova.res = aov(N ~ odmiany + warianty + rok, data = df)
summary(anova.res) # Zapisano do pliku

# Zadanie 2
df.filtered = df %>% filter(odmiany == "Paroli")
model = aov(GY ~ nawozenie, data = df.filtered)
TukeyHSD(model)

# Zadanie 3
df = readxl::read_excel("kukurydza.xlsx")
df.filtered = df %>% filter(odmiany == "Paroli")
names(df.filtered) = make.names(names(df.filtered))
model = '
GY ~ Bp + N + P + K + Ca + Mg + Na + N.K + N.P + N.Mg
'
fit = lavaan::sem(model, data = df.filtered)
semPlot::semPaths(fit, "std", "est", edge.label.cex = 0.5, layout = "spring", residuals = F)

# Zadanie 4
library(vegan)
df = read.table("kalusy-PCA.txt")
df.t = df %>% t()

df.dist = vegan::vegdist(df, method = "bray")
df.t.dist = vegan::vegdist(df.t, method = "bray")
hc = hclust(df.dist)
hc.t = hclust(df.t.dist)

par.save = par()
par(mfrow=c(1, 2))
plot(hc, main = "Klastrowanie roztworów")
plot(hc.t, main = "Klastrowanie genotypów")
par(par.save)

# Zadanie 5
df = read.table("kalusy-PCA.txt")
km = kmeans(df, centers = 3)
factoextra::fviz_cluster(km, data = df)
