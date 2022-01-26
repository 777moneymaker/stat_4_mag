library(tibble)
library(dplyr)
library(readxl)

euclid_linking_clust = function(df, method = "single") {
  dist.obj = dist(df, method = "euclidean")
  hclust(dist.obj, method = method)
}

bray_linking_clust = function(df, method = "single") {
  dist.obj = vegan::vegdist(df, method = "bray")
  hclust(dist.obj, method = method)
}

df = read_excel("Appendix1.xlsx") %>%
  column_to_rownames("Species") %>%
  t()

# 1
clust1 = euclid_linking_clust(df, "single")
clust2 = euclid_linking_clust(df, "complete")
clust3 = euclid_linking_clust(df, "average")
clust4 = euclid_linking_clust(df, "ward.D")
plot(clust1, main = "Euclid dist", xlab = "Single link", ylab = "Dist", sub = "")
plot(clust2, main = "Euclid dist", xlab = "Complete link", ylab = "Dist", sub = "")
plot(clust3, main = "Euclid dist", xlab = "Average link", ylab = "Dist", sub = "")
plot(clust4, main = "Euclid dist", xlab = "Ward.D link", ylab = "Dist", sub = "")

# 2
clust1 = bray_linking_clust(df, "single")
clust2 = bray_linking_clust(df, "complete")
clust3 = bray_linking_clust(df, "average")
clust4 = bray_linking_clust(df, "ward.D")
plot(clust1, main = "Bray dist", xlab = "Single link", ylab = "Dist", sub = "")
plot(clust2, main = "Bray dist", xlab = "Complete link", ylab = "Dist", sub = "")
plot(clust3, main = "Bray dist", xlab = "Average link", ylab = "Dist", sub = "")
plot(clust4, main = "Bray dist", xlab = "Ward.D link", ylab = "Dist", sub = "")

# 3
df = readr::read_table("kalusy-PCA.txt") %>%
  column_to_rownames("DC")

clust1 = euclid_linking_clust(df, "single")
clust2 = euclid_linking_clust(df, "complete")
clust3 = euclid_linking_clust(df, "average")
clust4 = euclid_linking_clust(df, "ward.D")
plot(clust1, main = "Euclid dist", xlab = "Single link", ylab = "Dist", sub = "")
plot(clust2, main = "Euclid dist", xlab = "Complete link", ylab = "Dist", sub = "")
plot(clust3, main = "Euclid dist", xlab = "Average link", ylab = "Dist", sub = "")
plot(clust4, main = "Euclid dist", xlab = "Ward.D link", ylab = "Dist", sub = "")

clust1 = bray_linking_clust(df, "single")
clust2 = bray_linking_clust(df, "complete")
clust3 = bray_linking_clust(df, "average")
clust4 = bray_linking_clust(df, "ward.D")
plot(clust1, main = "Bray dist", xlab = "Single link", ylab = "Dist", sub = "")
plot(clust2, main = "Bray dist", xlab = "Complete link", ylab = "Dist", sub = "")
plot(clust3, main = "Bray dist", xlab = "Average link", ylab = "Dist", sub = "")
plot(clust4, main = "Bray dist", xlab = "Ward.D link", ylab = "Dist", sub = "")
