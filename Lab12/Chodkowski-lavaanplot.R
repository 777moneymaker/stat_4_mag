library(lavaanPlot)
library(lavaan)

# Stworzenie kilku formuł regresji
# MPG objaśniane przez cyl + disp + hp
# oraz druga formuła regresji
# QSEC objasniane przez disp + hp + wt
model <- 'mpg ~ cyl + disp + hp
          qsec ~ disp + hp + wt'
# Obliczenia na podstawie powyższego modelu
fit1 <- sem(model, data = mtcars)

# Stworzenie sztucznych zmiennych objaśniających
# Visual - sztuczna zmienna powstała z  x1 + x2 + x3
# Textual - sztuczna zmienna powstałą z  x4 + x5 + x6
# Speed - sztuczna zmeinna powstałą z  x7 + x8 + x9
HS.model <- ' visual  =~ x1 + x2 + x3
textual =~ x4 + x5 + x6
speed   =~ x7 + x8 + x9
'
# Obliczenia na podstawie powyższego modelu
fit2 <- cfa(HS.model, data=HolzingerSwineford1939)

# Utworzenie wykresu, który modeluje objaśnianie zmiennych
lavaanPlot(model = fit1)

# Stworzenie wykresu, który modeluje sztuczne zmienne (z jakich zmiennych powstały)
lavaanPlot(model = fit2)

# Stworzenie tego samego wykresu modelującego objasniane zmienne,
# lecz tym razem z dodanymi etykietami
labels1 <- list(mpg = "Miles Per Gallon", cyl = "Cylinders", disp = "Displacement", hp = "Horsepower", qsec = "Speed", wt = "Weight")
lavaanPlot(model = fit1, labels = labels1)

# Stworzenie tego samego wykresu modelującego objaśniane zmienne,
# lecz tym razem z dodanymi etykietami oraz z
# przekazanymi dodatkowymi opcjami wykresu do biblioteki DiagrammeR
lavaanPlot(model = fit1, labels = labels1, node_options = list(shape = "box", fontname = "Helvetica"), edge_options = list(color = "grey"))

# Stworzenie wykresu analogicznego do poprzednich, ale z dodatkowymi opcjami
# Dodatkową opcją jest dodanie na wykres współczynników poszczególnych zmiennych
lavaanPlot(model = fit1, labels = labels1, coefs = TRUE)

# Wykres analogiczny do porzedniego, dodane współczynniki oraz standaryzacja współczynników
lavaanPlot(model = fit1, labels = labels1, coefs = TRUE, stand = TRUE)

# Wykres analogiczny, ale standaryzowane współczynniki są dodane tylko dla zmiennych dla których p.val < 0.05
lavaanPlot(model = fit1, labels = labels1, coefs = TRUE, stand = TRUE, sig = 0.05)


# Kroki analogiczne jak w modelu 'fit1'
# Stworzenie listy etykiet
labels = list(visual = "Visual Ability", textual = "Textual Ability", speed = "Speed Ability")
# Utworzenie wykresu z dodatkowymi opcjami i parametrami graficznymi, tj. czcionka, kształt, kolory itp.
# Dodanie współczynników
lavaanPlot(model = fit2, labels = labels, node_options = list(shape = "box", fontname = "Helvetica"), edge_options = list(color = "grey"), coefs = TRUE)
# Dodanie standaryzowanych współczynników
lavaanPlot(model = fit2, labels = labels, node_options = list(shape = "box", fontname = "Helvetica"), edge_options = list(color = "grey"), coefs = TRUE, stand = FALSE)
# Dodanie  współczynników i sig = 1.00
lavaanPlot(model = fit2, labels = labels, node_options = list(shape = "box", fontname = "Helvetica"), edge_options = list(color = "grey"), coefs = TRUE, stand = FALSE, sig = 1.00)


# Stworzenie modelu CFA ze sztucznymi zmiennymi
HS.model <- ' visual  =~ x1 + x2 + x3
textual =~ x4 + x5 + x6
speed   =~ x7 + x8 + x9
'
# Obliczenie/'fit' modelu na danych
fit <- cfa(HS.model, data=HolzingerSwineford1939)
# Dodanie etykiet do obliczonych danych
labels2 = list(visual = "Visual Ability", textual = "Textual Ability", speed = "Speed Ability")
# Stworzenie wykresu, na którym zamodelowane są sztuczne zmienne oraz ścieżki ukazujące kowariancje
lavaanPlot(model = fit2, labels = labels2, covs = TRUE)

# Stworzenie wykresu ścieżki z 'fit1' z dodanymi etykietami, współczynnikami,
# ścieżkami reprezentującymi kowariancje oraz z etykietami istotności współczynników
lavaanPlot(model = fit1, labels = labels1, coefs = TRUE, covs = TRUE, stars = c("regress"))

# Stworzenie wykresu modelującego sztuczne zmienne z dodanymi etykietami, współczynnikami, kowariancjami
# oraz z etykietami istotności współczynników
lavaanPlot(model = fit2, labels = labels2, coefs = TRUE, covs = TRUE, stars = c("latent"))

# Wykres analogiczny do powyższego, ale etykiety istotności są dodane dla współczynników oraz kowariancji
lavaanPlot(model = fit2, labels = labels2, coefs = TRUE, covs = TRUE, stars = c("latent", "covs"))
