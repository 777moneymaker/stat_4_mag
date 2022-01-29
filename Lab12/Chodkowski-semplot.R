library(semPlot)

#  Stworzenie ramki danych z losowych danych z rozkładu normalnego
X <- rnorm(100)
Y <- rnorm(100)
Z <- rnorm(1) * X + rnorm(1) * Y + rnorm(1) * X * Y
DF <- data.frame(X, Y, Z)

# Stworzenie modelu regresji z powyższych danych
# z uwzględnieniem interakcji zmiennych niezależnych
res <- lm(Z ~ X * Y, data = DF)

# Stworzenie diagramu ścieżki modelującego powyższy liniowy model z interakcją
semPaths(res)

# Stworzenie diagramu ściezki, gdzie krawędzie modelują standaryzowane parametry modelu
# a etykiety krawędzi są ukryte (brak liczb)
# Wykres może się różnić od strony, bo dane do lm() są generowane losowo
semPaths(res, "std", "hide")

library("lavaan")
# Wczytanie danych i nazwanie kolumn
Data <- read.table("http://www.statmodel.com/usersguide/chap5/ex5.8.dat")
names(Data) <- c(paste("y", 1:6, sep = ""), paste("x", 1:3, sep = ""))

# Stworzenie modelu CFA
# * Stworzenie sztucznych zmiennych f1 oraz f2
# * Użycie sztucznych zmiennych w modelu, gdzie sztuczne zmienne f1, f2
# są zmiennymi objaśnianymi przez zmienne x1, x2, x3
model.Lavaan <- "f1 =~ y1 + y2 + y3\nf2 =~ y4 + y5 + y6\nf1 + f2 ~ x1 + x2 + x3"

# Załadowanie biblioteki lavaan oraz obliczenie modelu cfa z 'model.lavaan'
library("lavaan")
fit <- lavaan::cfa(model.Lavaan, data = Data, std.lv = TRUE)

# Wykonanie diagramu ścieżki bez tytułu i z dodatkowymi opcjami graficznymi
# Diagram prezentuje graficznie cały model. Zaprezentowane są na nim :
# * sztuczne zmienne
# * interakcje
# * wariancje i kowariancje
semPaths(fit, title = FALSE, curvePivot = TRUE)

# Wykres analogiczny do powyższego, lecz grubość krawędzi
# odzwierciedla "wielkość" poszczególnych wartości (współczynników, wariancji, etc.)
semPaths(fit, "std", edge.label.cex = 0.5, curvePivot = TRUE)

# Analogiczne obliczenia, ale z danych pobranych z internetu
download.file("http://www.statmodel.com/usersguide/chap5/ex5.8.out", outfile <- tempfile(fileext = ".out"))
# Analogiczne stworzenie wykresu bezpośrednio z danych
semPaths(outfile, intercepts = FALSE)


# Pobranie danych z internetu i nadanie nazw kolumnom
Data <- read.table("http://www.statmodel.com/usersguide/chap5/ex5.2.dat")
names(Data) <- c("u1", "u2", "u3", "u4", "u5", "u6")
Data <- as.data.frame(lapply(Data, ordered))
# Stworzenie modelu
# F1 - sztuczna zmienna powstała z u1+u2+u3
# F2 - sztuczna zmienna powstała z u4+u5+u6
model <- " f1 =~ u1 + u2 + u3; f2 =~ u4 + u5 + u6 "
# Obliczenie modelu cfa z formuły powyżej
fit <- lavaan::cfa(model, data = Data)
# Stworzenie wykresu ścieżki z ukrytmi wyrazami wolnymi (Intercepts)
semPaths(fit, intercepts = FALSE)

# Analogiczny model, z danymi z pobranymi z internetu
download.file("http://www.statmodel.com/usersguide/chap5/ex5.2.out", outfile <- tempfile(fileext = ".out"))
# Diagram na pobranych danych
semPaths(outfile)


# Załadowanie biblioteki OpenMx
library("OpenMx")
# Wczytanie danych
data(demoOneFactor)
# Pobranie nazw z wczytanych danych
manifests <- names(demoOneFactor)
# Zdefiniowanie zmiennej sztucznej G
latents <- c("G")
# Stworzenie jednoczynnikowego modelu CFA za pomocą OpenMx z odpowiednimi opcjami
model <- mxModel("One Factor", type = "RAM", manifestVars = manifests, latentVars = latents,
                 mxPath(from = latents, to = manifests), mxPath(from = manifests, arrows = 2),
                 mxPath(from = latents, arrows = 2, free = FALSE, values = 1), mxData(cov(demoOneFactor),
                                                                                      type = "cov", numObs = 500))
# Uruchomienie obliczeń danego modelu
model <- mxRun(model)

# Stworzenie diagramu ściezki dla obliczonego modelu z odpowiednimi kolorami
# Diagram obrazuje sztuczną zmienną G
semPaths(model, color = list(lat = rgb(245, 253, 118, maxColorValue = 255),
                             man = rgb(155, 253, 175, maxColorValue = 255)), mar = c(10, 5, 10, 5))


# === Analiza pominięta, serwer nie odpowiada
source("http://openmx.psyc.virginia.edu/svn/trunk/demo/TwoFactorModel_PathCov.R")
semPaths(twoFactorFit, layout = "tree2")
# ===

# Pobranie pliku z danymi
download.file("http://sachaepskamp.com/files/mi1.OUT", modFile <- tempfile(fileext = ".OUT"))
# Ustawienie layoutu graficznego
layout(t(1:2))
# Stworzenie diagramu ścieżki
# Model jest przedstawiony jako graf bez wag krawędzi
# Kolory oznaczają 'constraints?' - nie wiem jak to interpretować
#
semPaths(modFile, "eq", ask = FALSE, as.expression = "edges", mar = c(3, 1,
                                                                      5, 1))



# Wczytanie danych z internetu
Data <- read.table("http://www.statmodel.com/usersguide/chap5/ex5.8.dat")
# Nazwanie kolumn
names(Data) <- c(paste("y", 1:6, sep = ""), paste("x", 1:3, sep = ""))

# Stworzenie modelu, określenie sztucznych zmiennych
# oraz liniowej regresji na sztucznych zmiennych
model.Lavaan <- "f1 =~ y1 + y2 + y3\nf2 =~ y4 + y5 + y6\nf1 + f2 ~ x1 + x2 + x3 "

# Obliczenie modelu CFA z powyższej formuły
library("lavaan")
fit.Lavaan <- lavaan::cfa(model.Lavaan, data = Data, std.lv = TRUE)

# 'Wyciągniecie' wszystkich formuł z modelu
model.Lavaan2 <- semSyntax(fit.Lavaan, "lavaan")

# Stworzenie modelu z wyciągniętych formuł powyżej
fit.Lavaan2 <- lavaan::lavaan(model.Lavaan2, data = Data)

# Ustawienie layout i porównanie modelu stworzonego z pobranych danych bezpośrednio
# z modelem stworzonym z od postawy z wyciągniętych danych
layout(t(1:2))
semPaths(fit.Lavaan, "std", title = FALSE)
title("Lavaan model 1", line = 3)
semPaths(fit.Lavaan2, "std", title = FALSE)
title("Lavaan model 2", line = 3)

# Wyciągnięcie 'syntaxu'/ formuł z modelu w określonym formacie 'sem'
model.sem <- semSyntax(fit.Lavaan, syntax = "sem")

# Załadowanie biblioteki sem oraz stworzenie modelu sem
library("sem")
fit.sem <- sem:::sem(model.sem, data = Data)

# Porównanie modeli stworzonych dwoma róznymi metodami
layout(t(1:2))
semPaths(fit.Lavaan, "std", title = FALSE)
title("Lavaan", line = 3)
semPaths(fit.sem, "std", title = FALSE)
title("sem", line = 3)



# Swtorzenie przykładowej macierzy
Loadings <- rbind(diag(1, 2, 2), diag(1, 2, 2), diag(1, 2, 2))
# Swtorzenie sztucznej zmiennej???
LatVar <- diag(1, 2, 2)

# Kolejne macierze
Beta <- matrix(0, 2, 2)
Beta[1, 2] <- 1
ManVar <- diag(1, nrow(Loadings), nrow(Loadings))
Gamma <- diag(1, 2, 2)
ManInts <- rep(1, 6)
LatInts <- rep(1, 2)

# stworzenie modelu 'lisrel' z przygotowanych wcześniej macierzy
mod <- lisrelModel(LY = Loadings, PS = LatVar, BE = Beta, TE = ManVar, LX = Loadings,
                   PH = LatVar, GA = Gamma, TD = ManVar, TY = ManInts, TX = ManInts, AL = LatInts,
                   KA = LatInts)

# Stworzenie wykresu ścieżki z graficznymi parametrai
semPaths(mod, as.expression = c("nodes", "edges"), sizeMan = 3, sizeInt = 1,
         sizeLat = 4)
semPaths(mod, as.expression = c("nodes", "edges"), sizeMan = 3, sizeInt = 1,
         sizeLat = 4, label.prop = 0.5, curve = 0.5, bg = "black", groups = "latents",
         intercepts = FALSE, borders = FALSE, label.norm = "O")


# Losowy zbiór danych z rozkładu normalnego
A <- rnorm(100)
B <- A + rnorm(100)
C <- B + rnorm(100)
DF <- data.frame(A, B, C)

# Stworzenie dwóch modeli regresji, jednoczynnikowa i dwuczynnikowa bez interakcji
res1 <- lm(B ~ C, data = DF)
res2 <- lm(A ~ B + C, data = DF)

# Stworzenie diagramu powyższych modeli
semPaths(res1 + res2, "model", "est", intercepts = FALSE)
semPaths(list(res1, res2), "model", "est", intercepts = FALSE)

# Pokazanie opisu klasy semPlotModel
showClass("semPlotModel")
