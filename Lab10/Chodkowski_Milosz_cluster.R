library(tibble)
library(dplyr)

df = readxl::read_excel("dziesiecioboj-1.xlsx") %>% column_to_rownames("Athlets")
dd = dist(scale(df), method = "euclidean")
hc = hclust(dd, method = "ward.D2")
plot(hc)
plot(hc, hang = -1, cex = 0.6)
hcd <- as.dendrogram(hc)
# Default plot
plot(hcd, type = "rectangle", ylab = "Height")
plot(hcd, type = "triangle", ylab = "Height")
plot(hcd, xlim = c(1, 20), ylim = c(1,8))
nodePar <- list(lab.cex = 0.6, pch = c(NA, 19),
                cex = 0.7, col = "blue")
# Customized plot; remove labels
plot(hcd, ylab = "Height", nodePar = nodePar, leaflab = "none")
plot(hcd,  xlab = "Height",
     nodePar = nodePar, horiz = TRUE)
plot(hcd,  xlab = "Height", nodePar = nodePar,
     edgePar = list(col = 2:3, lwd = 2:1))

library("ape")
plot(as.phylo(hc), cex = 0.6, label.offset = 0.5)
plot(as.phylo(hc), type = "cladogram", cex = 0.6,
     label.offset = 0.5)
plot(as.phylo(hc), type = "unrooted", cex = 0.6,
     no.margin = TRUE)
plot(as.phylo(hc), type = "fan")
plot(as.phylo(hc), type = "radial")

colors = c("red", "blue", "green", "black")
clus4 = cutree(hc, 4)
plot(as.phylo(hc), type = "fan", tip.color = colors[clus4],
     label.offset = 1, cex = 0.7)
plot(as.phylo(hc), type = "cladogram", cex = 0.6,
     edge.color = "steelblue", edge.width = 2, edge.lty = 2,
     tip.color = "steelblue")

library("ggplot2")
library("ggdendro")

ggdendrogram(hc)
ggdendrogram(hc, rotate = TRUE, theme_dendro = FALSE)
dend <- as.dendrogram(hc)
dend_data <- dendro_data(dend, type = "rectangle")
names(dend_data)

p <- ggplot(dend_data$segments) +
    geom_segment(aes(x = x, y = y, xend = xend, yend = yend))+
    geom_text(data = dend_data$labels, aes(x, y, label = label),
              hjust = 1, angle = 90, size = 3)+
    ylim(-3, 15)
print(p)
