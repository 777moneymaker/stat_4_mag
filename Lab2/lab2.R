#!/usr/bin/env Rscript
# 1
A = c(6.7, 7.3, 8, 8, 7.9, 9.2, 10.1)
B = c(7.5, 7.7, 7.7, 8.2, 8.9, 8.9, 10.6)
C = c(5.9, 6.9, 7, 7, 9.5, 9.6, 9.6)

df = data.frame(A, B, C)
rowMeans(df)
colMeans(df)

# 2
rzepak.tbl = read.table('rzepak.txt', header = T)
rzepak.tbl
colMeans(rzepak.tbl)
rowMeans(rzepak.tbl)

rzepak.csv = read.csv("rzepak.csv", header = T)
rzepak.csv
colMeans(rzepak.csv)
rowMeans(rzepak.csv)


