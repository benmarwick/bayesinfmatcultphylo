

prepare_arrowhead_data_for_input_to_revbayes_fn <- function(
                                                            arrowhead_outlines,
                                                            arrowhead_locations,
                                                            arrowhead_typochronology
                                                            ){

## if we are not using targets, we can import the data with these
# arrowhead_outlines =       here::here("analysis/data/raw_data/outlines_combined_nicholas_2016.RDS")
#,
# arrowhead_locations =      here::here("analysis/data/raw_data/nicolas_fleches_2016_catalog_ids_with_coordinates.csv")
#,
# arrowhead_typochronology = here::here("analysis/data/raw_data/nicolas_2017_typochronologie.csv")
# ){


# from https://github.com/yesdavid/designspace_culttax_article_2021/blob/main/2_script/3_code_late_neolithic_early_bronze_age.R
# with minor modifications

library(here)
library(cluster)
library(ggplot2)
library(readr)
library(Momocs)
library(dplyr)
library(maptree)
library(ggimage)
library(phangorn)
library(rworldmap)
library(raster)
library(NbClust)

if (!requireNamespace("ggtree", quietly = TRUE)){
  if (!requireNamespace("BiocManager", quietly = TRUE)){
    install.packages("BiocManager")
    BiocManager::install("ggtree")
    library(ggtree)
  } else {
    BiocManager::install("ggtree")
    library(ggtree)
  }
} else {library(ggtree)}


outlines_combined_nicolas_2016 <- readRDS(arrowhead_outlines)

outlines_combined_nicolas_2016_centered <-
  Momocs::coo_centre(outlines_combined_nicolas_2016) # center
outlines_combined_nicolas_2016_centered_scaled <-
  Momocs::coo_scale(outlines_combined_nicolas_2016_centered) # scale
outlines_combined_nicolas_2016_centered_scaled <-
  Momocs::coo_slidedirection(outlines_combined_nicolas_2016_centered_scaled,
                             direction = "up")

# unification of outlines with catalogue-dataframe
nicolas_fleches_2016_catalog_ids_coordinates <-
  readr::read_csv(file = arrowhead_locations)

outlines_combined_nicolas_2016_names <-
  names(outlines_combined_nicolas_2016_centered_scaled)
outlines_combined_nicolas_2016_names_splitted <-
  strsplit(outlines_combined_nicolas_2016_names, split = "_")

ID_and_artefact_ID_list <- list()
for (name_index in 1:length(outlines_combined_nicolas_2016_names)){

  ID_and_artefact_ID_interrim_df <- data.frame(ID = paste0(outlines_combined_nicolas_2016_names_splitted[[name_index]][1], "-", outlines_combined_nicolas_2016_names_splitted[[name_index]][2]),
                                               ID_artefact <- outlines_combined_nicolas_2016_names[[name_index]])
  names(ID_and_artefact_ID_interrim_df) <- c("ID", "ID_artefact")
  ID_and_artefact_ID_list[[name_index]] <- ID_and_artefact_ID_interrim_df

}

ID_and_artefact_ID_df <-
  do.call("rbind", ID_and_artefact_ID_list)

nicolas_fleches_2016_catalog_ids_coordinates_artefact_ID <-
  dplyr::inner_join(ID_and_artefact_ID_df,
                    nicolas_fleches_2016_catalog_ids_coordinates,
                    by = "ID")

write_csv(nicolas_fleches_2016_catalog_ids_coordinates_artefact_ID,
          here("analysis/data/derived_data/nicolas_fleches_2016_catalog_ids_coordinates_artefact_ID.csv"))

outlines_combined_nicolas_2016_centered_scaled <-
  Momocs::Out(outlines_combined_nicolas_2016_centered_scaled$coo,
              fac = nicolas_fleches_2016_catalog_ids_coordinates_artefact_ID)

outlines_combined_nicolas_2016_centered_scaled <-
  Momocs::filter(outlines_combined_nicolas_2016_centered_scaled,
                 !ID_artefact %in% c("UK_60_XX_pseudo_no_10",
                                     "UK_15_XX_pseudo_no_4"))
# remove fragmented outliers

n_artefacts_denmark <- length(which(nicolas_fleches_2016_catalog_ids_coordinates_artefact_ID$country == "Denmark")) # number of artefacts from denmark
n_artefacts_france <- length(which(nicolas_fleches_2016_catalog_ids_coordinates_artefact_ID$country == "France")) # number of artefacts from france
n_artefacts_uk <- length(which(nicolas_fleches_2016_catalog_ids_coordinates_artefact_ID$country == "United Kingdom")) # number of artefacts from the uk

# harmonic calibration
outlines_combined_nicolas_2016_centered_scaled_harmonics <-
  Momocs::calibrate_harmonicpower_efourier(outlines_combined_nicolas_2016_centered_scaled,
                                           plot = F)  # Estimates the number of harmonics required for the Fourier methods implemented in Momocs.

# efourier
outlines_combined_nicolas_2016_centered_scaled_efourier <-
  Momocs::efourier(outlines_combined_nicolas_2016_centered_scaled,
                   nb.h = as.matrix(outlines_combined_nicolas_2016_centered_scaled_harmonics[["minh"]])[[4,1]], # harmonics for 99.9%
                   norm = F) # you selected `norm=TRUE`, which is not recommended. See ?efourier --> probably no problem in our case


# PCA
outlines_combined_nicolas_2016_centered_scaled_PCA <-
  Momocs::PCA(outlines_combined_nicolas_2016_centered_scaled_efourier) # PCA on Coe objects, using prcomp.

minimum_no_of_pcs_nicolas <-
  Momocs::scree_min(outlines_combined_nicolas_2016_centered_scaled_PCA,
                    prop = 0.95) # minimum number of axis to use to retain a given proportion (i.e. prop = 0.99 to describe 99% of the variation) -- reduces computing time in the phylogeny estimation step:


# plot not shown here

#### remove outliers
nicolas_outliers_db <-
  fpc::dbscan(outlines_combined_nicolas_2016_centered_scaled_PCA$x,
              eps = 0.3,
              MinPts = 3)

# plot not shown here

nicolas_outliers_cluster <-
  data.frame(name = row.names(outlines_combined_nicolas_2016_centered_scaled_PCA$x),
             value = nicolas_outliers_db$cluster,
             row.names = NULL)

nicolas_outliers_cluster_outlier_names <-
  subset(nicolas_outliers_cluster, value != 1)

outlines_combined_nicolas_2016_centered_scaled_PCA$fac$outlier_names <- NA
outlines_combined_nicolas_2016_centered_scaled$fac$cluster <- as.factor(nicolas_outliers_cluster$value)

for (vector_index in 1:length(match(nicolas_outliers_cluster_outlier_names$name,
                                    outlines_combined_nicolas_2016_centered_scaled_PCA$fac$ID_artefact))){

  current_index <- match(nicolas_outliers_cluster_outlier_names$name,
                         outlines_combined_nicolas_2016_centered_scaled_PCA$fac$ID_artefact)[vector_index]

  outlines_combined_nicolas_2016_centered_scaled_PCA$fac$outlier_names[current_index] <- nicolas_outliers_cluster_outlier_names$name[vector_index]
}

# look at the shape of the outliers
# BM: Momocs::slice doesn't work in a function, no idea why, so I use the more
# primative function
x1 <- nicolas_outliers_cluster_outlier_names$name
x2 <- outlines_combined_nicolas_2016_centered_scaled_PCA$fac$ID_artefact
idx_1 <- base::match(x1, x2)
z <- 1+1

nicolas_2016_with_outliers <-
  Momocs:::subsetize.Coo(outlines_combined_nicolas_2016_centered_scaled, idx_1 )

# plot not shown here

# analysis without outliers
y1 <- nicolas_outliers_cluster_outlier_names$name
y2 <- outlines_combined_nicolas_2016_centered_scaled_PCA$fac$ID_artefact
idx_2 <- base::match(y1, y2)
idx_2 <- (-idx_2)

nicolas_2016_without_outliers <-
  Momocs:::subsetize.Coo(outlines_combined_nicolas_2016_centered_scaled, idx_2 )


# harmonic calibration
nicolas_2016_without_outliers_harmonics <-
  Momocs::calibrate_harmonicpower_efourier(nicolas_2016_without_outliers,
                                           plot = F)  # Estimates the number of harmonics required for the Fourier methods implemented in Momocs. This is the only step in this section that produces data we need in the subsequent step.

n_harmonics_for_99.9 <- as.matrix(nicolas_2016_without_outliers_harmonics[["minh"]])[[4,1]]
write_lines(n_harmonics_for_99.9,
            here::here("analysis/data/derived_data/n_harmonics_for_99.9.txt"))

# efourier
nicolas_2016_without_outliers_efourier <-
  Momocs::efourier(nicolas_2016_without_outliers,
                   nb.h = n_harmonics_for_99.9, # harmonics for 99.9%
                   norm = F)
# PCA
nicolas_2016_without_outliers_PCA <-
  Momocs::PCA(nicolas_2016_without_outliers_efourier) # PCA on Coe objects, using prcomp.

nicolas_2016_screeplot_wo_outliers <-
  Momocs::scree_plot(nicolas_2016_without_outliers_PCA,
                     nax = 1:50)

# see how many PCs will give 99.9%  39 PCs gets there
pca_summary <- summary(nicolas_2016_without_outliers_PCA)
pc_for_99 <- unname(which(pca_summary$importance[3, ] >= 0.99)[1])
write_lines(pc_for_99,
            here::here("analysis/data/derived_data/pc_for_99.txt"))

minimum_no_of_pcs_nicolas_without_outliers <- pc_for_99

gg_nicolas <- Momocs::PCcontrib(nicolas_2016_without_outliers_PCA,
                                nax = 1:5,
                                sd.r = c(-2,-1,0,1,2))

library(ggplot2)
gg_nicolas <- gg_nicolas$gg +
  theme_bw() +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

ggsave(here("analysis/figures/nicolas_arrowheads_pca_contrib.png"),
       h = 10, w = 8)

# PCA plot not shown

############################# typochronology #############################

typochronologie_csv <- readr::read_csv(arrowhead_typochronology)

typochronologie_csv <- dplyr::distinct(typochronologie_csv, ID_country, .keep_all = T)

typochronologie_csv_FR <- subset(typochronologie_csv, Country == "FR")

nicolas_2016_without_outliers_PCA_as_df <-
  as.data.frame(nicolas_2016_without_outliers_PCA$x[,1:minimum_no_of_pcs_nicolas_without_outliers])

nicolas_2016_without_outliers_PCA_as_df$ID_country <- sapply(rownames(nicolas_2016_without_outliers_PCA_as_df),
                                                             function(x){
                                                               strsplit(x, split = "(?<=.{5})", perl = TRUE)[[1]][1] # RegEx "(?<=.{5})" means: split into chunks of 5 characters long
                                                             })

########## FR
nicolas_2016_without_outliers_PCA_as_df_subset_typochron_FR <- subset(nicolas_2016_without_outliers_PCA_as_df, ID_country %in% typochronologie_csv_FR$ID_country)

names_artefacts_ID <- data.frame(artefact_ID = rownames(nicolas_2016_without_outliers_PCA_as_df_subset_typochron_FR),
                                 ID_country = nicolas_2016_without_outliers_PCA_as_df_subset_typochron_FR$ID_country)
names_artefacts_ID_and_period <- dplyr::left_join(names_artefacts_ID, typochronologie_csv_FR[,c("ID_country", "Period")], by = "ID_country")
names_artefacts_ID_and_period$Period <- as.factor(names_artefacts_ID_and_period$Period)

# plot not shown

nicolas_2016_without_outliers_PCA_as_df_subset_typochron_FR$ID_country <- NULL

nicolas_typochron_FR_dist <- dist(nicolas_2016_without_outliers_PCA_as_df_subset_typochron_FR, method = "euclidean")

additional_information_period <- data.frame(Period = names_artefacts_ID_and_period[,c("Period")])
rownames(additional_information_period) <- names_artefacts_ID_and_period$artefact_ID

# end of script from  https://github.com/yesdavid/designspace_culttax_article_2021/blob/main/2_script/3_code_late_neolithic_early_bronze_age.R
# with minor modifications.

# Now we attach country and chrono data, only possible for FR data, ready to
# input into RevBayes

library(stringr)
library(dplyr)
library(tibble)

names_artefacts_ID_and_period_clean <-
  names_artefacts_ID_and_period %>%
  mutate(old_artefact_ID = artefact_ID) %>%
  mutate(artefact_ID = str_remove(artefact_ID, "_pseudo_no")) %>%
  mutate(artefact_ID = str_remove_all(artefact_ID, "_XX")) %>%
  tidyr::unite('country_and_period',
               ID_country, Period,
               sep = "_",
               remove = FALSE) %>%
  # taxa names must be first col for ggtree to work with %<+%
  mutate(taxa = artefact_ID ) %>%
  relocate(taxa) %>%
  as_tibble %>%
  # make the taxa strings equal length with some padding
  mutate(taxa =
   str_split_fixed(taxa, "_", 4) %>%
   as_tibble() %>%
   mutate(V3 = ifelse(nchar(V3) < 2,  paste0("0", V3), V3),
          V4 = ifelse(nchar(V4) == 1, paste0("0", V4), V4),
          V4 = ifelse(nchar(V4) == 2, V4, "00")) %>%
   tidyr::unite(V1,  V2,  V3,  V4, sep = "_") %>%
   pull(V1) %>%
   paste0("FR_", .))

# update the artefact IDs on the PCA row labels

nicolas_2016_without_outliers_PCA_as_df_subset_typochron_FR_clean <-
nicolas_2016_without_outliers_PCA_as_df_subset_typochron_FR %>%
  rownames_to_column() %>%
  left_join(names_artefacts_ID_and_period_clean,
            join_by(rowname == old_artefact_ID)) %>%
  mutate(period =
           case_match(
             Period,
             "Bell Beaker" ~  "Bell-Beaker",
             "Bronze A1 1" ~  "EBA-1",
             "Bronze A2 2" ~  "EBA-2",
             "Bronze A2 3" ~  "EBA-3",
           ),
         taxa = paste0(taxa, "_", period )) %>%
  dplyr::select(-rowname,
         -artefact_ID,
         -country_and_period,
         -ID_country,
         -Period,
         -period
         ) %>%
  column_to_rownames("taxa")


data_for_revbayes <- nicolas_2016_without_outliers_PCA_as_df_subset_typochron_FR_clean %>%
  # only want columns that have PC values
  dplyr::select(starts_with("PC"))

# write a nex file for input into RevBayes,
write.nexus.data(as.matrix(data_for_revbayes),
                 file.path(here("analysis/data/derived_data/data_for_revbayes.nex")),
                 format = "continuous")

# save this for Ancestral State Reconstruction
data_for_revbayes %>%
  as.data.frame() %>%
  rownames_to_column() %>%
  write_csv(here("analysis/data/derived_data/data_for_revbayes.csv"))

}





