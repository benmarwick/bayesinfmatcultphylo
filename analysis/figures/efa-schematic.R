library("FactoMineR")
library(factoextra)

iris.pca <- PCA(iris[,-5], graph = FALSE)

fviz_pca_ind(iris.pca,
             label="none",
             habillage=iris$Species,
             addEllipses=TRUE,
             ellipse.level=0.95,
             col.ind = "white",
             fill.ind = "white",
             col.ind.sup = "white",
             alpha.ind = 0,
             palette = NULL) +
  ggtitle("") +
  guides(colour = 'none',
         shape = 'none',
         fill = 'none') +
  coord_fixed() +
  theme_bw(base_size = 12)
