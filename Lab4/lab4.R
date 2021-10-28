#!/usr/bin/env Rscript

# library(openxlsx)

# 1
df = data.frame(
    rod_a = c(3.8,
        3.7,
        2.9,
        3.5,
        2.6,
        3.3,
        0,
        0),
    rod_b = c(3.7,
        4.6,
        5.4,
        6.2,
        4.2,
        3.5,
        5.3,
        5.5)
)
# Próby niezależne
shapiro.test(df$rod_a)
shapiro.test(df$rod_b)
# Testy normalności wykazały, że próba rod_A jest niezgodna z rozkładem normalnym,
# a próba rod_B jest z nim zgodna.
# Test Wilcoxona
# H0: u_a - u_b = 0
# H1: u_a - u_b != 0
with(df, {
    wilcox.test(rod_a, rod_b)
})
# p.value < 0.05 (0.003798), a więc mamy podstawy do przyjęcia hipotezy alternatywnej.
# Możemy więc uznać, że średnie z dwóch populacji są od siebie różne. =>
# różnica w średnich jest istotna.

# 2
df = data.frame(
    metoda_g = c(
        2.73,
        2.84,
        3.18,
        2.79,
        3.05,
        3.03,
        3.10,
        2.88,
        3.00,
        3.07,
        2.66,
        2.78,
        3.62,
        3.31,
        2.71,
        2.80,
        2.95,
        3.52
        
    ),
    metoda_b = c(
        2.88,
        2.93,
        3.38,
        2.99,
        3.30,
        3.19,
        3.34,
        3.08,
        3.20,
        3.23,
        2.81,
        2.94,
        3.59,
        3.41,
        2.88,
        2.99,
        3.16,
        3.66
    )
)
# Próby są zależne
# Testy normalności
shapiro.test(df$metoda_g) # > 0.05
shapiro.test(df$metoda_b) # > 0.05
# Obie próby zgodne z rozkładem normalnym.
# Test wariancji
var.test(df$metoda_g, df$metoda_b) # p.val > 0.05 => wariancje równe
# H0: u_g = u_b
# H1: u_g != u_b
with(df, {
    t.test(metoda_g,
        metoda_b,
        paired = T,
        var.equal = T)
}) # p.val < 0.05 (4.651e-09)
# p.val < 0.05 co daje nam podstawy do odrzucenia hipotezy zerowej i przyjęcia
# hipotezy alternatywnej. Uznajemy więc, że badane metody dają odmienne wyniki.

# 3
ulica = c(98, 116, 100, 103, 104, 102, 105, 99, 106, 101)
park = c(109, 118, 121, 108, 115, 111, 110, 113, 107, 117)
df = data.frame(ulica, park)
# Próby są niezależne
# H0: u_park = u_ulica
# H1: u_park != u_ulica
# Testy normalności
shapiro.test(df$ulica)
shapiro.test(df$park)
# Założenie o normalności rozkładu nie jest spełnione. Jedna z prób go nie spełnia.
with(df, {
    wilcox.test(ulica, park)
}) # p.val < 0.05 (0.0004871)
# p.value < 0.05, co pozwala nam przyjąć hipotezę alternatywną.
# Z tego wynika, że lokalizacja drzewa ma statystycznie istotny wpływ na wysokość.

# 4
stare_paliwo = c(1039, 1168, 1008, 1035, 1035, 1025, 1059, 1012, 1012, 1039)
nowe_paliwo = c(1096, 1161, 1210, 1088, 1154, 1111, 1103, 1094, 1059, 1177)
df = data.frame(nowe_paliwo, stare_paliwo)
# Dane niezależne
# Test normalności
shapiro.test(df$stare_paliwo) # => p.val < 0.05
shapiro.test(df$nowe_paliwo) # p.val > 0.05
# Założenie o zgodności z rozkładem normalnym nie jest spełnione.
# H0: u_stare = u_nowe
# H1: u_stare < u_nowe
with(df, {
    wilcox.test(stare_paliwo, nowe_paliwo, paired = F, alternative = 'less')
}) # p.val < 0.05 (0.0009547)
# P.value < 0.05, co pozwala przyjąć hipotezę alternatywną, która mówi, że
# nowe paliwo ma wpływ na wzrost przejechanej drogi.

# 5
studenci = c(9, 12, 19, 21, 24)
studentki = c(11, 16, 20, 23)
# Dane niezależne
# test normalności
shapiro.test(studenci)
shapiro.test(studentki)
# Obie próby są zgodne z rozkładem normalnym
# Testy wariancji
var.test(studenci, studentki) # p.val > 0.05. Wariancje są równe
# H0: u_studenci = u_studentki
# H1: u_studenci > u_studentki
t.test(studenci, studentki, paired = F, var.equal = T) # p.val > 0.05 (0.9021)
# p.val > 0.05, zatem nie ma podstaw do odrzucenia hipotezy zerowej. 
# Z tego wynika, że studenci nie są lepsi od studentek pod względem zdolności matematycznych.

# 6
A = c(6.7, 7.3, 8.0, 8.0, 7.9, 9.2, 10.1, 9.2, 8.3, 8.4, 8.0, 7.9)
B = c(7.5, 7.7, 7.7, 8.2, 8.9, 8.9, 10.6, 10.2, 9.4, 9.4, 8.2, 7.8)
C = c(5.9, 6.9, 7.0, 7.0, 9.5, 9.6, 9.6, 10.3, 8.1, 8.5, 8.6, 8.8)
df = data.frame(A, B, C)
# Dane niezależne
# Test wariancji
p.vals = lapply(df, function(x){shapiro.test(x)$p.value})
all(p.vals > 0.05) # => TRUE. Wszystkie p.val > 0.05. 
# Zgodność z rozkładem normalnym w każdej próbie.
# Test wariancji
bartlett.test(df) # => p.value > 0.05. Wariancje są homogeniczne/równe.
aov()
library(reshape2)
 



