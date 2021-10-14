#!/usrbin/env Rscript

# 1
tomatos = c(1.52, 1.57, 1.30, 1.62, 1.55, 1.70, 2.05, 1.64, 1.95, 1.80, 1.76, 1.40, 1.92, 2.20, 1.57, 1.59, 1.27, 1.79, 1.29, 1.84, 1.77, 1.72, 1.53, 1.32, 1.69, 1.95, 1.75, 1.08, 1.70, 1.45)
summary(tomatos)

png("barplot.png")
barplot(tomatos)
dev.off()

png("plot.png")
plot(tomatos)
dev.off()

png("histogram.png")
hist(tomatos)
dev.off()

png("boxplot.png")
boxplot(tomatos)
dev.off()

# 2
# a
A = c(6.7, 7.3, 8, 8, 7.9, 9.2, 10.1, 9.2, 8.3, 8.4, 8, 7.9)
B = c(7.5, 7.7, 7.7, 8.2, 8.9, 8.9, 10.6, 10.2, 9.4, 9.4, 8.2, 7.8)
C = c(5.9, 6.9, 7, 7, 9.5, 9.6, 9.6, 10.3, 8.1, 8.5, 8.6, 8.8)
# b & c
zyto = data.frame(A = A, B = B, C = C)
zyto
# d
zyto[, 1][1:3]
# e
zyto$C[1:2]
# f
zyto$B
# g
zyto$B[1:2]
# h
boxplot(zyto)
png("gatunki.png")
dev.off()

# 3
# a
func = function(x) (x + 1) / (x^2 - 4)
png("wykres1.png")
curve(func, from = -5, to = 5)
dev.off()
# b
png("wykres2.png")
curve(func, from = -5, to = 5)
abline(v = 0, h = 0, lty = 2)
dev.off()
# c
png("wykres3.png")
curve(func, from = -5, to = 5)
abline(v = c(0, -2, 2), h = 0, lty = 2)
dev.off()

# 4
png("sincos1.png")
curve(sin, from = -3 * pi, to = 3 * pi, col = 'red')
curve(cos, from = -3 * pi, to = 3 * pi, add = T, col = 'green')
dev.off()

# 5
func1 = function(x) x^2
func2 = function(x) (x - 2)^2
func3 = function(x) (x - 2)^2 + 3
func4 = function(x) x^2 + 3
func5 = function(x) (x +1)^2 - 2

curve(func1, -5, 5, col = 'green', ylim = c(-15, 25))
curve(func2, -5, 5, col = 'red', add = T)
curve(func3, -5, 5, col = 'blue', add = T)
curve(func4, -5, 5, col = 'orange', add = T)
curve(func5, -5, 5, col = 'magenta', add = T)
abline(v = 0, h = 0, col = 'black')
legend("bottomleft", 
       legend = c("Func1", "Func2", "Func3", "Func4", "Func5"), 
       col = c('green', 'red', 'blue', 'orange', 'magenta'), 
       pch = 16,
       text.col = "black")
title("Wykresy funkcji przesuniętych")
# punkty ..

# 6
library(ggplot2)
p = ggplot(data = TitanicSurvival, aes(survived, fill = passengerClass)) + geom_histogram(stat='count')
p

# 7