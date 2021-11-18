library(ggplot2)
library(dplyr)
library(tidyr)

# 1
df = data.frame(maka = seq(from = 15, to = 50, by = 5),
    cukier = seq(from = 15, to = 50, by = 5))
res = with(df, {
    cor.test(maka, cukier)
})
# p.val = 2.2e-16
# cor = 1

# 2
df = data.frame(
    A = c(0.6, .2, .3, .7, .5, .8, .7, .4, .6, .8),
    B = c(1.5, 1.5, 1.1, 1.2, 1.4, 1.1, 1.6, 1.0, 1.9, 1.7)
)
with(df, {
    cor.test(A, B)
})

# 3
tbl = as.table(rbind(c(29, 194), c(17, 68)))
dimnames(tbl) = list(parch = c("C", "P"), owocowka = c("U", "N"))
chisq.test(tbl)
# p.val = 0.17 -> Nie odrzucamy H0, zmienne są niezależne.

# 4
tbl = as.table(rbind(
    c(21, 41, 93, 47),
    c(33, 37, 35, 53),
    c(45, 75, 27, 43),
    c(30, 48, 50, 55),
    c(71, 47, 49, 50)
))
dimnames(tbl) = list(
    zarobki = c("<500", "500-1000", "1000-1500", "1500-2000", ">2000"),
    wyksztalcenie = c("Podstawowe", "Średnie", "Wyższe", "Ponad wyższe")
)
chisq.test(tbl)
# p.val = 8.913e-15 -> p.value pozwala na odrzucenie hipotezy zerowej
# Z tego wynika że istnieje zależnośc pomiędzy wykształcenien a zarobkami.

# 5
tbl = as.table(cbind(c(4, 14, 17, 19, 6),
    c(6, 16, 21, 13, 4)))
rownames(tbl) = c(3, 4, 5, 6, 7)
colnames(tbl) = c("Kawa Szatanex", "Kawa Lukrecja")
fisher.test(tbl)
# p.val = 0.6696 -> Nie odrzucamy hipotezy zerowej. Nie istnieje żadna zależnośc między ocenami obu kaw.

# 6
df = data.frame(
    dni_przechowywania = c(1, 2, 3, 4, 5, 6, 7),
    zawartosc_wody = c(49, 47.5, 47, 46.5, 44.5, 44, 45)
)
with(df, {
    model = lm(zawartosc_wody ~ dni_przechowywania)
    print(summary(model))
    coefs = round(as.vector(summary(model)$coefficients[, 1]), 2)
    cat("Rownanie:\n")
    cat(paste("y = ", coefs[2], "x + ", coefs[1], sep = ""))
    cat("\nKorelacja:\n")
    print(cor.test(dni_przechowywania, zawartosc_wody))
    ggplot(df, aes(dni_przechowywania, zawartosc_wody)) + geom_point() + geom_smooth(method =
            "lm")
})

# 7
df = data.frame(l_sprzedawcow = c(1, 2, 3, 4, 5, 6, 6),
    obroty = c(1.2, 2.2, 3, 3.5, 4, 5, 6))
with(df, {
    model = lm(obroty ~ l_sprzedawcow)
    print(summary(model))
    coefs = round(as.vector(summary(model)$coefficients[, 1]), 2)
    cat("Rownanie:\n")
    cat(paste("y = ", coefs[2], "x + ", coefs[1], sep = ""))
    cat("\nKorelacja:\n")
    print(cor.test(l_sprzedawcow, obroty, method = "spearman"))
    ggplot(df, aes(l_sprzedawcow, obroty)) + geom_point() + geom_smooth(method =
            "lm")
})

# 8
df = data.frame(
    zawartosc = c(2.0, 2.5, 2.9, 3.6, 4.0, 4.5, 5.0),
    kalorie = c(40, 48, 50, 52, 60, 65, 68)
)
with(df, {
    model = lm(kalorie ~ zawartosc)
    print(summary(model))
    coefs = round(as.vector(summary(model)$coefficients[, 1]), 2)
    cat("Rownanie:\n")
    cat(paste("y = ", coefs[2], "x + ", coefs[1], sep = ""))
})

# 9
df = tibble(czas_moczenia = c(.5, 1, 2, 5, 10),
    straty = c(5, 7, 9, 11, 13))
with(df, {
    model = lm(straty ~ czas_moczenia)
    print(summary(model))
    coefs = round(as.vector(summary(model)$coefficients[, 1]), 2)
    cat("Rownanie:\n")
    cat(paste("y = ", coefs[2], "x + ", coefs[1], sep = ""))
    cat("\nKorelacja:\n")
    print(cor.test(czas_moczenia, straty, method = "pearson"))
    ggplot(df, aes(czas_moczenia, straty)) + geom_point() + geom_smooth(method =
            "lm")
})

# 10
df = tibble(czas = c(5, 6, 10, 2, 8, 9),
    sprzedaz = c(2, 2.1, 2.5, 1.6, 2.3, 2.5))
with(df, {
    model = lm(sprzedaz ~ czas)
    print(summary(model))
    coefs = round(as.vector(summary(model)$coefficients[, 1]), 2)
    cat("Rownanie:\n")
    cat(paste("y = ", coefs[2], "x + ", coefs[1], sep = ""))
})

# 11
df = readr::read_tsv(file.choose())
df = df %>% select(-lp)
with(df, {
    model = lm(Plon ~ DlugoscLuszczyn + ZawartoscTluszczu + SumaGlukozynolanow)
    print(summary(model))
    coefs = round(as.vector(summary(model)$coefficients[, 1]), 2)
    
    cat("Rownanie:\n")
    cat(paste(
        "y = ", 
        coefs[2], "*DlugoscLuszczyn + ",
        coefs[3], "*ZawartoscTluszczu + ",
        coefs[4], "*SumaGlukozynolanow + ",
        coefs[1],
        sep = ""))
    ggplot(df %>% tidyr::pivot_longer(!matches("Plon"), names_to = "Cecha")) + geom_point(aes(Plon, value, color=Cecha)) + geom_smooth(method = "lm", aes(Plon, value, color=Cecha))
})

# Excel
df = read.csv(text="x1	y1	x2	y2	x3	y3	x4	y4
10	8,04	10	9,14	10	7,46	8	6,58
8	6,95	8	8,14	8	6,77	8	5,76
13	7,58	13	8,74	13	12,74	8	7,71
9	8,81	9	8,77	9	7,11	8	8,84
11	8,33	11	9,26	11	7,81	8	8,47
14	9,96	14	8,1	14	8,84	8	7,04
6	7,24	6	6,13	6	6,08	8	5,25
4	4,26	4	3,1	4	5,39	19	12,5
12	10,84	12	9,13	12	8,15	8	5,56
7	4,82	7	7,26	7	6,42	8	7,91
5	5,68	5	4,74	5	5,73	8	6,89", sep = '\t', dec=',') %>% tibble()

# mean
df %>% summarise_all(mean)
# var
df %>% summarise_all(var)

# df = df %>% tibble() %>% tidyr::gather() %>% mutate(key = gsub("[a-z]", "", x = key), value = as.double(value)) %>% group_by(key) %>% mutate(row = row_number()) %>% tidyr::pivot_wider(names_from = key, values_from = value) %>% select(-row)
