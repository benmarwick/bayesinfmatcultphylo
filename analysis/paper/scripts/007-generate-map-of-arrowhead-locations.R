

generate_map_of_arrowhead_locations_fn <- function(){


library(sf)
# inset world map, from
# https://stackoverflow.com/a/62104396/1036500
library(maps)
library(magrittr)
library(maptools)
library(raster)
library(ggplot2)
library(tidyverse)

#Defining a general CRS
mycrs <- "+proj=longlat +datum=WGS84 +no_defs"

#Using the original maps package, then converting map into SpatialPolygons object
world <- maps::map("world", fill=TRUE,  plot = FALSE) %$%
  maptools::map2SpatialPolygons(., IDs=names,proj4string=CRS(mycrs))

#The resulting map has self intersection problems so any further operation reports errors; using buffers of width 0 is a fast fix
while(rgeos::gIsValid(world)==FALSE){
  world <- rgeos::gBuffer(world, byid = TRUE, width = 0, quadsegs = 5, capStyle = "ROUND")
}

#Dissolving polygon's limits
world <- raster::aggregate(world)

#Plotting. I add theme_void to your code to erase any axis, etc
worldplot <-
  ggplot() +
  geom_polygon(data = world,
               aes(x=long, y=lat,
                   group=group),
               fill='NA',
               color='black',
               size=0.1) +
  annotate("rect",
           xmin = -10,
           xmax = 5,
           ymin = 40,
           ymax = 55,
           fill = NA,
           colour = "red",
           size = 0.5
  ) +
  theme_void() +
  coord_fixed(1) +
  labs(x = "", y = "") +
  labs(x = NULL, y = NULL, title = NULL) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  theme(panel.background = element_rect(fill = "white"),
        panel.border = element_blank(),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())


nicolas_fleches_2016_catalog_ids_coordinates_artefact_ID <-
read_csv(here::here("analysis/data/derived_data/nicolas_fleches_2016_catalog_ids_coordinates_artefact_ID.csv"))


nicolas_fleches_2016_catalog_ids_coordinates_sf <-
  nicolas_fleches_2016_catalog_ids_coordinates_artefact_ID %>%
  filter(country_code == "FR") %>%
  mutate(lon = lng) %>%
  st_as_sf(coords = c("lat", "lon"),
           remove = FALSE)


library(ggmap)
library(ggplot2)
fr <- c(-5.5,
        46.5,
        0.5,
        50)
fr_base_map <- get_stamenmap(fr,
              zoom = 8) %>%
  ggmap()

library(ggrepel)
map_of_sites <-
fr_base_map +
  geom_point(data = nicolas_fleches_2016_catalog_ids_coordinates_sf %>%
               distinct(ID, .keep_all = TRUE),
             aes(x = lon,
                 y = lat),
          size = 1,
          colour = "red") +
  geom_text_repel(data = nicolas_fleches_2016_catalog_ids_coordinates_sf %>%
                    distinct(ID, .keep_all = TRUE),
                  aes(x = lon,
                      y = lat,
                      label = ID),
                  max.overlaps = Inf,
                  segment.size = 0.1,
                  segment.color = "grey20",
                  size = 2,
                  color = "white",     # text color
                  bg.color = "grey20", # shadow color
                  bg.r = 0.15          # shadow radius
                  )


# combine site map and inset map
library(cowplot)
ggdraw() +
  draw_plot(map_of_sites) +
  draw_plot(plot = worldplot,
            x = 0.64, # x location of inset placement
            y = -0.06, # y location of inset placement
            width = .4, # Inset width
            height = .35, # Inset height
            scale = 0.5 # Inset scale
  ) +
  theme(panel.background = element_rect(fill='white', colour="white"),
        plot.background =  element_rect(fill='white', colour="white")) +
  labs(x = "Longitude",
       y = "Latitude")


ggsave(here::here("analysis/figures/fig-map-arrow-head-locations.png"),
       h = 8,
       w = 10)

}


# #------------------------------------------------------------------
# # http://blog.phytools.org/2019/03/projecting-phylogenetic-tree-onto-map.html
# library(phytools)
#
# library(mapdata)
# ## here are my data
# map_tree_for_plotting = ape::read.nexus(here::here("analysis/data/derived_data/output-002/map_tree.nex"))
#
# nicolas_fleches_2016_catalog_ids_coordinates_sf_id <-
# nicolas_fleches_2016_catalog_ids_coordinates_sf %>%
#   mutate(ID1 = str_remove(ID_artefact, "_pseudo_no")) %>%
#   mutate(ID1 = str_remove(ID1, "XX_")) %>%
#   separate(ID1, c("num", "num1", "num2", "num3"), "_") %>%
#   mutate(num2 = ifelse(nchar(num2) < 2,  paste0("0", num2), num2),
#          num3 = ifelse(nchar(num3) == 1, paste0("0", num3), num3),
#          num3 = ifelse(is.na(num3),  "00", num3)) %>%
#   tidyr::unite("ID", num, num1, num2,  num3,  sep = "_") %>%
#   as_tibble()
#
# map_tree_for_plotting_tip.label_tbl <-
# map_tree_for_plotting$tip.label %>%
#   tibble(tip_label = .) %>%
#   separate(tip_label, c("num", "num1", "num2", "num3", "num4"), "_") %>%
#   unite(ID, c("num", "num1", "num2", "num3"), sep = "_") %>%
#   arrange(ID) %>%
#   left_join(nicolas_fleches_2016_catalog_ids_coordinates_sf_id) %>%
#   unite(ID, c("ID", "num4"), sep = "_") %>%
#   dplyr::select(ID, lat, long = lon) %>%
#   column_to_rownames("ID") %>%
#   as.data.frame()
#
# library(viridis)
# cols <- setNames(sample(viridis(n=Ntip(map_tree_for_plotting))),
#                  map_tree_for_plotting$tip.label)
#
# obj <- phylo.to.map(tree = map_tree_for_plotting,
#                     coords = map_tree_for_plotting_tip.label_tbl,
#                     database="worldHires",
#                     xlim = c(-6, -2),
#                     ylim = c(46, 49),
#                     direction="downwards",
#                     plot=FALSE)
#
# # this takes such a long time!
# plot(
#   obj,
#   direction = "downwards",
#   colors = cols,
#   cex.points = c(0, 1),
#   lwd = c(1, 1),
#   ftype = "off",
#   fsize = 0.5,
# )
#
