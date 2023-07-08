
generate_phylogeny_figures_fn <- function(map_tree_for_plotting,
                                          treespace_for_plotting,
                                          data_for_revbayes) {


# https://thibautjombart.github.io/treespace/articles/introduction.html
library("treespace")
library("adegenet")
library("adegraphics")
library("rgl")
library(cowplot)
library(phylosignal)
library(adephylo)
library(ape)
library(phylobase)
library(tidyverse)
library(ggtree)

# for saving figures
dpi <- 600

# if we are not using targets, we can load the data here
# map_tree_for_plotting = here::here("analysis/data/derived_data/output-002/map_tree.nex")
# treespace_for_plotting = here::here("analysis/data/derived_data/output-002/tree_trace.trees")
# data_for_revbayes_csv = here::here("analysis/data/derived_data/data_for_revbayes.csv")

outsumfile <- ape::read.nexus(map_tree_for_plotting)
data_for_revbayes <- read_csv(data_for_revbayes_csv)

#-------------------------------------------------
# Summary of treespace
# read in the tree space
phy_raw <- ape::read.tree(treespace_for_plotting)
phy_raw <- di2multi(phy_raw, tol = 1e-08) # remove -ve branch lengths
phy <- phy_raw[1:1000] # small sample while exploring and polishing, because this is very slow

# use treespace
res <- treespace::treespace(phy, nf=3)

wm.groves <- findGroves(res, nclust = 3)
# capture this to use later
plotGroves(wm.groves$treespace$pco,
           point.cex = 2)
plotGroves_plot <- grid::grid.grab(wrap.grobs = TRUE)

# take a look and save it
plot_grid(plotGroves_plot)

ggsave(here::here("analysis/figures/treespace-map.png"),
       w = 10,
       h = 10,
       dpi = dpi)

#----------------------------------------------------------------
# ggtree densitree method
densitree_plot <-
ggdensitree(phy,
            tip.order = outsumfile$tip.label,
            alpha= .01,
            colour='steelblue') +
  theme_tree() +
  geom_tiplab(size=1.5) +
  xlim(-1, 0.08) +
  theme(legend.position = c(0.1, 0.92),
        legend.spacing.y = unit(0.5, 'mm'),
        legend.key.size = unit(4, "mm"),
        legend.title=element_text(size = 12),
        legend.text=element_text(size = 12))

# take a look and save it, this take a few minutes
densitree_plot

ggsave(here::here("analysis/figures/densitree.png"),
       w = 12,
       h = 10,
       dpi = dpi)

#-------------------------------------------------------------------
## inspecting phylogenetic signal

# https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/j.2041-210X.2012.00196.x
# https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5312541/
# https://wiki.duke.edu/pages/viewpage.action?pageId=131172281

tre <- outsumfile
tre$edge.length[which(tre$edge.length <= 0)] <- 0.0000001
dat <- data_for_revbayes %>%
  column_to_rownames("rowname")
p4d <- phylo4d(tre, dat)

phyloSignal_list <-
  phyloSignal(p4d = p4d, method = "all", reps = 9999)

lambda_for_each_trait <-
  tibble(Lambda = phyloSignal_list$stat$Lambda,
         p = phyloSignal_list$pvalue$Lambda,
         PC = factor(paste0("PC", 1:length(phyloSignal_list$stat$Lambda)))) %>%
  mutate(PC = factor(PC, levels = PC) )

ggplot(lambda_for_each_trait) +
  aes(x = PC,
      y = Lambda) +
  geom_col() +
  labs(x = "",
       y = "Pagel's Lambda") +
  theme_minimal(base_size = 12)

ggsave(here::here("analysis/figures/pagels-lamdba.png"),
       w = 20.6,
       h = 3,
       bg = "white",
       dpi = dpi)

arrow.lipa <- lipaMoran(p4d)

arrow.lipa_pc_list <- list()
for(i in 1:ncol(arrow.lipa$lipa)){
  arrow.lipa_pc_list[[i]] <-
    tibble(lipa = data.frame(arrow.lipa$lipa)[ , i],
           p =   (data.frame(arrow.lipa$p.value)[ , i] < 0.05) + 1)
}
arrow.lipa_pc_list_tbl <-
  bind_rows(arrow.lipa_pc_list, .id = "PC")

png(here::here("analysis/figures/phylo-signal-plot.png"),
    w = 22,
    h = 10,
    units = "in",
    res = dpi)

barplot.phylo4d(p4d,
                bar.col=(arrow.lipa$p.value < 0.05) + 1,
                bar.lwd = 2,
                tip.cex = 0.6,
                trait.cex = 0.7,
                trait.bg.col = "white",
                grid.vertical = FALSE,
                grid.horizontal = FALSE,
                center = FALSE ,
                scale = FALSE)

dev.off()

#-------------------------------------------------
# compose multiple PNG files into single panels of figures

# combine the png files of the images
# we are using imagemagik, not an R pkg, on OSX install with
# brew install imagemagick

system(paste0("convert +append ",
              here::here("analysis/figures/treespace-map.png"),
              " ",
              here::here("analysis/figures/densitree.png"),
              " ",
              "-resize x2000 -density 1000 -quality 99 ",
              here::here("analysis/figures/treespace-densitree.png")))

# https://stackoverflow.com/questions/41415370/combining-images-vertically-with-a-horizontal-offset
system(paste0("convert  ",
              here::here("analysis/figures/phylo-signal-plot.png"),
              " ",      # w x h
              "\\( -size 275x500 xc:white ",
              here::here("analysis/figures/pagels-lamdba.png"),
              " +append  \\) ",
              "-append  -density 1000 -quality 99  ",
              here::here("analysis/figures/phylo-signal-lamdba.png")))

system(paste0("convert -append ",
              here::here("analysis/figures/treespace-densitree.png"),
              " ",
              here::here("analysis/figures/phylo-signal-lamdba.png"),
              " ",
              "-resize 2000x -density 1000 -quality 99  ",
              here::here("analysis/figures/treespace-densitree-map-signal-lamdba.png")))

###----------------------------------------------------------
# #  visualize the branch-specific rates by plotting them using
# R package RevGadgets.

outsumfile_trees <- RevGadgets::readTrees(here::here("analysis/data/derived_data/output-05/map_tree.nex"))

outsumfile_trees_phy <- outsumfile_trees[[1]][[1]]

outsumfile_trees_phy@phylo$edge.length[which(outsumfile_trees_phy@phylo$edge.length <= 0)] <- 0.0000001

# capitalize and remove hyphens
.titleFormat <- function(string) {
  string <- gsub("-", " ", string)
  string <- gsub("_", " ", string)
  substr(string, 1, 1) <- toupper(substr(string, 1, 1))
  return(string)
}

branch_color = viridis::viridis(n = 4,
                                begin = 0 ,
                                end = 1,
                                direction = -1)

pp <- ggtree(outsumfile_trees_phy)

col_num <- which(colnames(pp$data) == "branch_rates")
pp$data[, col_num] <-
  as.numeric(as.data.frame(pp$data)[, col_num])
name <- .titleFormat("branch_rates")
pp1 <-
  ggtree(outsumfile_trees_phy) +
  aes(color = as.data.frame(pp$data)[, col_num])  +
  ggplot2::scale_color_gradient(
    low = branch_color[1],
    high = branch_color[2],
    breaks = pretty(as.data.frame(pp$data)[, col_num]),
    name = name
  ) +
  theme(legend.position = c(0.1, 0.92),
        legend.spacing.y = unit(0.5, 'mm'),
        legend.key.size = unit(4, "mm"),
        legend.title=element_text(size = 12),
        legend.text=element_text(size = 10))


pp1  +
   geom_tiplab(size=1.5,
               colour = "black",
               align = TRUE) +
  xlim(-0.1, 1.2)

ggsave(here::here("analysis/figures/tree-branch-rates.png"),
       w = 10,
       h = 10,
       dpi = dpi)

#-----------------------------------------------------------------
# Ancestral state reconstruction

# from http://www.phytools.org/eqg2015/asr.html
library(phytools)

## read tree from file
anc.tree <- outsumfile

## read data
anc.data <- dat # data_for_revbayes

## change this into a vector
anc.data.1.5  <- as.matrix(anc.data)[,1:5]

# Now we can estimate ancestral states. We will also compute variances & 95%
# confidence intervals for each node:

# continuous character mapping and 2D phylomorphospaces using a
# type of phylogenetic scatterplot.

x <-
  fancyTree(anc.tree,
            type="scattergram",
            X=anc.data.1.5,
            fsize = 0.1)

# update the colour scheme
for(i in 1:length(x$contMaps)){
  n <- length(x$contMaps[[i]]$cols)
  x$contMaps[[i]]$cols[1:n] <- viridis::viridis(n)
}

# redraw with new colours
png(here::here("analysis/figures/phylomorphospace-panel.png"),
    width = 10,
    height = 10,
    units = "in",
    res = dpi)
plot(x, fsize = 0.1)
dev.off()

system(paste0("convert +append ",
              here::here("analysis/figures/tree-branch-rates.png"),
              " ",
              here::here("analysis/figures/phylomorphospace-panel.png"),
              " ",
              "-resize x2000 -density 1000 -quality 99 ",
              here::here("analysis/figures/tree-rates-morphospace.png")))

}








