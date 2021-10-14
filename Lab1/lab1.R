#!/usr/bin/env Rscript

# Task 1
3 + 5 # 1.1
4 - 6 # 1.2
8 * 7 # 1.3
21 / 5 # 1.4
5 ** 3 # 1.5
sqrt(49) # 1.6
8 ** 1 / 3 # 1.7
log(7) # 1.8
log(6, 10) # 1.9
log2(5) # 1.10
log(4, 5) # 1.11
exp(3) # 1.12
sin(2 * pi) # 1.13
sin(pi / 6) # 1.14
sin(pi / 3) # 1.15
cos(pi / 2) # 1.16
cos(pi / 4) # 1.17

# Task 3
# 2.1
x = 8
print(x)

# 2.2
imie = "Jan"
print(imie)

# 2.3
nazwisko = "Kowalski"
print(nazwisko)

# Task 3
# 3.1
a = c(1, 3, 5)
print(a)
# 3.2
b = 3:14
print(b)
# 3.3
ab = c(a, b)
print(ab)
# 3.4
ab[6:10] = c(0,-6,-3,-1,-5)
# 3.5
wek1 = rep(1:2, 3)
print(wek1)
# 3.6
wek2 = rep(1:2, each = 3)
print(wek2)
# 3.7
wek3 = seq(1, 10, by = 2)
print(wek3)
# 3.8
wek4 = seq(101, 110, by = 2)
print(wek4)
# 3.9
wek34 = wek3 - wek4
print(wek34)
# 3.10
wek43 = wek4 - wek3
print(wek43)
# 3.11
wek5 = wek3 + wek4
print(wek5)
# 3.12
wek6 = wek4 / wek3
print(wek6)
# 3.13
wek7 = (5 * wek4) + (6 * wek3)
print(wek7)
# 3.14
wek8 = log(wek4) + cos(wek3)
print(wek8)

# Task 4
# 4.1
x = 1:7
print(x)
# 4.2
x[5]
# 4.3
x[2:6]
# 4.4
x[c(2, 4)]
# 4.5
x[x < 4]

# Task 5
# a
rep(1, 8)
# b
rep(c(1,4), 7)
# c
rep(c(3, 6), times = c(8, 3))
# d
rep(5:1, 1:5)
# e
rep(c(12,21, 43), times = c(3, 1, 2))
# f
rep(c("A", "B"), 3)
# g
rep(seq(1, 11, by=2), each = 2)

# Task 6
plon = c(1.52, 1.57, 1.3, 1.62, 1.55, 1.7, 2.05, 1.64, 1.95, 1.8, 1.76, 1.40, 1.92, 2.2)
plon
min(plon)
max(plon)
range(plon)
sum(plon)
prod(plon)
mean(plon)
median(plon)
var(plon)
sd(plon)

plon.sorted = sort(plon)
summary(plon.sorted)
