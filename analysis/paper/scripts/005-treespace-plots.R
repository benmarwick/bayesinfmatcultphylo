# https://thibautjombart.github.io/treespace/articles/introduction.html
library("treespace")
library("adegenet")
library("adegraphics")
library("rgl")
library(cowplot)

outsumfile <- ape::read.nexus(here::here("analysis/data/derived_data/output-01/map_tree.nex"))

# read in the tree space
phy <- ape::read.tree(here::here("analysis/data/derived_data/output-01/tree_trace.trees" ) )
phy <- di2multi(phy, tol = 1e-08) # remove -ve branch lengths
phy <- phy[1:500]

# use treespace
res <- treespace::treespace(phy, nf=3)

# this is only possible if we are looking at the FR subset, the other
# regions don't have so much chrono data
tip_labels <-
  # made by 001
  names_artefacts_ID_and_period %>%
  mutate(artefact_ID = str_remove(artefact_ID, "_pseudo_no")) %>%
  mutate(artefact_ID = str_remove_all(artefact_ID, "_XX")) %>%
  tidyr::unite('country_and_period',
               ID_country, Period,
               sep = "_",
               remove = FALSE) %>%
  # taxa names must be first col for ggtree to work with %<+%
  mutate(taxa = row.names(data_for_revbayes) ) %>%
  relocate(taxa) %>%
  as_tibble

wm.groves <- findGroves(res, nclust = 3)
# capture this to use later
plotGroves(wm.groves$treespace$pco)
plotGroves_plot <- grid::grid.grab(wrap.grobs = TRUE)

# https://thibautjombart.github.io/treespace/articles/introduction.html

outsumfile_tree_plot <-
ggtree(outsumfile) %<+% tip_labels +  # names_artefacts_ID_and_period_unite +
  theme_tree() +
  geom_tiplab(size=2,
              aes(label = ID_country,
                  color = Period)) +
  geom_treescale()  +
  coord_cartesian(clip = 'off') +
  theme_tree2(plot.margin=margin(6, 120, 6, 6)) +
  theme(legend.position = c(0.2, 0.95),
       legend.spacing.y = unit(0.1, 'mm'),
       legend.key.size = unit(1, "mm"),
       legend.title=element_text(size = 6),
       legend.text=element_text(size = 4))

# ggtree densitree method
densitree_plot <-
ggdensitree(phy,
            alpha= .01,
            colour='steelblue') %<+% tip_labels +
  theme_tree() +
  geom_tiplab(size=2,
              aes(label = ID_country,
                  color = Period)) +
  geom_treescale() +
  guides(label = "none",
         color = "none")

#-------------------------------------------------
# single panel
library(cowplot)
plot_grid(plotGroves_plot,
          outsumfile_tree_plot,
          densitree_plot,
          align = "v",
          axis = "tb",
          rel_widths = c(3, 3, 5),
          ncol = 3)

ggsave(here::here("analysis/figures/treespace-map-densitree.png"),
      w = 7,
      h = 5)


