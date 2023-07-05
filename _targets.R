# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# BM: we run this sequence using
# targets::tar_make()
# in the R console

# Load packages required to define the pipeline:
library(targets)
library(tarchetypes) # Load other packages as needed.
options(crayon.enabled = FALSE, tidyverse.quiet = TRUE)

# Set target options:
tar_option_set(
  format = "qs" # need to install the qs package
)

# Run the R scripts with my custom functions:

# load the functions from our script files
my_script_files <- list.files(here::here("analysis/paper/scripts"),
                              pattern = ".R$",
                              full.names = TRUE)
purrr::walk(my_script_files, source)


list(
  #------------------------------------------------------------------------
  # make figure for bibliometric summary
  tar_target(
    name = biblio_data_file,
    here::here("analysis/data/raw_data/Archaeology phylogenetics case studies.xlsx"),
    format = "file"
    ),
  tar_target(
    name = bilbiometrics_figure,
    command = bilbiometrics_figure_fn(biblio_data_file)
  ),
  #------------------------------------------------------------------------
  # identify files that we need to read in to prepare the data for revbayes
  # this will ensure they are monitored for changes
  tar_target(
    name = arrowhead_outlines,
    here::here("analysis/data/raw_data/outlines_combined_nicholas_2016.RDS"),
    format = "file"
  ),
  tar_target(
    name = arrowhead_locations,
    here::here("analysis/data/raw_data/nicolas_fleches_2016_catalog_ids_with_coordinates.csv"),
    format = "file"
  ),
  tar_target(
    name = arrowhead_typochronology,
    here::here("analysis/data/raw_data/nicolas_2017_typochronologie.csv"),
    format = "file"
  ),
  # run the code to do the EFA and subset French arrowheads to prepare for input
  # into revbayes
  tar_target(
    name = prepare_arrowhead_data_for_input_to_revbayes,
    command = prepare_arrowhead_data_for_input_to_revbayes_fn(arrowhead_outlines,
                                                              arrowhead_locations,
                                                              arrowhead_typochronology)
  ),
  # monitor the data that will go into revbayes for changes
  tar_target(
    name = data_for_revbayes_csv,
    here::here("analysis/data/derived_data/data_for_revbayes.csv"),
    format = "file"
  ),
  # monitor the data that will go into revbayes for changes
  tar_target(
    name = data_for_revbayes_nex,
    here::here("analysis/data/derived_data/data_for_revbayes.nex"),
    format = "file"
  ),
  #------------------------------------------------------------------------
  # run the revbayes scripts, these are R functions that use system() to
  # run revbayes. We also have the Rev script files are targets to ensure
  # they are monitored and re-run when we change them
  tar_target(
    name = revbayes_script_01,
    command = here::here("analysis/paper/scripts/004-01-RevBayes-for-MAP.Rev"),
    format = "file"
  ),
  tar_target(
    name = run_revbayes_script_01,
    command = run_revbayes_script_01_fn(revbayes_script_01,
                                        data_for_revbayes_nex
                                        )
  ),
  tar_target(
    name = revbayes_script_02,
    command = here::here("analysis/paper/scripts/004-02-RevBayes-for-MAP.Rev"),
    format = "file"
  ),
  tar_target(
    name = run_revbayes_script_02,
    command = run_revbayes_script_02_fn(revbayes_script_02,
                                        data_for_revbayes_nex
    )
  ),
  tar_target(
    name = revbayes_script_03,
    command = here::here("analysis/paper/scripts/004-03-RevBayes-for-MAP.Rev"),
    format = "file"
  ),
  tar_target(
    name = run_revbayes_script_03,
    command = run_revbayes_script_03_fn(revbayes_script_03,
                                        data_for_revbayes_nex
    )
  ),
  tar_target(
    name = revbayes_script_04,
    command = here::here("analysis/paper/scripts/004-04-RevBayes-for-MAP.Rev"),
    format = "file"
  ),
  tar_target(
    name = run_revbayes_script_04,
    command = run_revbayes_script_04_fn(revbayes_script_04,
                                        data_for_revbayes_nex
    )
  ),
  tar_target(
    name = revbayes_script_05,
    command = here::here("analysis/paper/scripts/004-05-RevBayes-for-MAP.Rev"),
    format = "file"
  ),
  tar_target(
    name = run_revbayes_script_05,
    command = run_revbayes_script_05_fn(revbayes_script_05,
                                        data_for_revbayes_nex
    )
  ),
  #------------------------------------------------------------------------
  # Generate figures summarising the results of the MCMC that we use in the
  # paper
  tar_target(
    name = map_tree_for_plotting,
    command = here::here("analysis/data/derived_data/output-02/map_tree.nex"),
    format = "file"
  ),
  tar_target(
    name = treespace_for_plotting,
    command = here::here("analysis/data/derived_data/output-02/tree_trace.trees"),
    format = "file"
  ),

  tar_target(
    name = generate_phylogeny_figures,
    command = generate_phylogeny_figures_fn(map_tree_for_plotting,
                                            treespace_for_plotting,
                                            data_for_revbayes_csv)
  ),
  #------------------------------------------------------------------------
  # Summarise the characteristics of the models and their
  # bayes factors into a small table
  tar_target(
    name = make_summary_table_of_models,
    command = make_summary_table_of_models_fn()
  ),
  #------------------------------------------------------------------------
  # Finally, render the Quarto document into a Word document, drawing on the
  # figures and tables we generated with the above functions. There is a
  # tar_quarto but it didn't work for me
  tar_target(
    name = report,
    command =  quarto::quarto_render(here::here("analysis/paper/paper.qmd"))
 )
)
