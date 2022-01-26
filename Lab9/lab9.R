# Analiza składowych głównych

# 1
library(dplyr)
setwd("~/My stuff/stat_4_mag/Lab9/")
df = readxl::read_excel("dziesiecioboj-1.xlsx")
df = df %>% tibble::column_to_rownames("Athlets")
df.pca = FactoMineR::PCA(df, scale.unit = T, graph = F)
# 1.1
factoextra::get_eig(df.pca)
# 1.2
factoextra::fviz_eig(df.pca)
# 1.3
factoextra::fviz_pca_var(df.pca)
# 1.4
factoextra::fviz_pca_var(df.pca,
                         col.var = "cos2",
                         repel = T,
                         gradient.cols = "Paired")
# 1.5
factoextra::fviz_pca_var(df.pca, col.var = "contrib", repel = T)
# 1.6
factoextra::fviz_pca_ind(df.pca)
# 1.7
factoextra::fviz_pca_ind(df.pca, col.ind = "cos2")
# 1.8
factoextra::fviz_pca_ind(df.pca, col.ind = "contrib")
# 1.9
factoextra::fviz_pca_biplot(df.pca, ggtheme = theme_minimal())

# 2
# 2.1
df = readr::read_table("eucarpia2.txt")
df = df %>% select(-rok)

df = df %>%
    group_by(pozywka, genotyp) %>%
    mutate(s = sum(kalusy, zielone, albinosy)) %>%
    select(pozywka, genotyp, s) %>%
    unique() %>%
    tidyr::pivot_wider(id_cols = pozywka,
                       names_from = genotyp,
                       values_from = s) %>%
    tibble::column_to_rownames("pozywka")

df.pca = FactoMineR::PCA(df, graph = F)
factoextra::fviz_pca_biplot(df.pca, col.var = "cos2", gradient.cols = hcl.colors(2, "Warm"), addEllipses = T, repel = T)
