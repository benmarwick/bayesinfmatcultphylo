# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# BM: we run this sequence using
# targets::tar_make()
# in the R console

# BM: to run individual targets, run
# targets::tar_load()

# Load packages required to define the pipeline:
library(targets)
library(tarchetypes) # Load other packages as needed.
options(crayon.enabled = FALSE,
         tidyverse.quiet = TRUE,
         memory = "transient",
         garbage_collection = TRUE)

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
    name = bilbiometrics_figure_function,
    command = bilbiometrics_figure_fn(biblio_data_file)
  ),
  #------------------------------------------------------------------------
  # identify files that we need to read in to prepare the data for revbayes
  # this will ensure they are monitored for changes
  tar_target(
    name = arrowhead_outlines_file,
    here::here("analysis/data/raw_data/outlines_combined_nicholas_2016.RDS"),
    format = "file"
  ),
  tar_target(
    name = arrowhead_locations_file,
    here::here("analysis/data/raw_data/nicolas_fleches_2016_catalog_ids_with_coordinates.csv"),
    format = "file"
  ),
  tar_target(
    name = arrowhead_typochronology_file,
    here::here("analysis/data/raw_data/nicolas_2017_typochronologie.csv"),
    format = "file"
  ),
  # run the code to do the EFA and subset French arrowheads to prepare for input
  # into revbayes
  tar_target(
    name = prepare_arrowhead_data_for_input_to_revbayes_function,
    command = prepare_arrowhead_data_for_input_to_revbayes_fn(arrowhead_outlines_file,
                                                              arrowhead_locations_file,
                                                              arrowhead_typochronology_file)
  ),
  # monitor the data that will go into revbayes for changes
  tar_target(
    name = data_for_revbayes_csv_file,
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
    name = revbayes_script_001_file,
    command = here::here("analysis/paper/scripts/004-001-RevBayes-for-MAP.Rev"),
    format = "file"
  ),
  tar_target(
    name = run_revbayes_script_001_function,
    command = run_revbayes_script_001_fn(revbayes_script_001_file,
                                        data_for_revbayes_nex
                                        )
  ),
  tar_target(
    name = revbayes_script_002_file,
    command = here::here("analysis/paper/scripts/004-002-RevBayes-for-MAP.Rev"),
    format = "file"
  ),
  tar_target(
    name = run_revbayes_script_002_function,
    command = run_revbayes_script_002_fn(revbayes_script_002_file,
                                        data_for_revbayes_nex
    )
  ),
  tar_target(
    name = revbayes_script_003_file,
    command = here::here("analysis/paper/scripts/004-003-RevBayes-for-MAP.Rev"),
    format = "file"
  ),
  tar_target(
    name = run_revbayes_script_003_function,
    command = run_revbayes_script_003_fn(revbayes_script_003_file,
                                        data_for_revbayes_nex
    )
  ),
  tar_target(
    name = revbayes_script_004_file,
    command = here::here("analysis/paper/scripts/004-004-RevBayes-for-MAP.Rev"),
    format = "file"
  ),
  tar_target(
    name = run_revbayes_script_004_function,
    command = run_revbayes_script_004_fn(revbayes_script_004_file,
                                        data_for_revbayes_nex
    )
  ),
  tar_target(
    name = revbayes_script_005_file,
    command = here::here("analysis/paper/scripts/004-005-RevBayes-for-MAP.Rev"),
    format = "file"
  ),
  tar_target(
    name = run_revbayes_script_005_function,
    command = run_revbayes_script_005_fn(revbayes_script_005_file,
                                        data_for_revbayes_nex
    )
  ),
  #------------------------------------------------------------------------
  # Generate figures summarising the results of the MCMC that we use in the
  # paper
  tar_target(
    name = map_tree_for_plotting_file,
    command = here::here("analysis/data/derived_data/output-002/map_tree.nex"),
    format = "file"
  ),
  tar_target(
    name = treespace_for_plotting_file,
    command = here::here("analysis/data/derived_data/output-002/tree_trace.trees"),
    format = "file"
  ),

  tar_target(
    name = generate_phylogeny_figures,
    command = generate_phylogeny_figures_fn(map_tree_for_plotting_file,
                                            treespace_for_plotting_file,
                                            data_for_revbayes_csv_file)
  ),
  #------------------------------------------------------------------------
  # Summarise the characteristics of the models and their
  # bayes factors into a small table
  tar_target(
    name = make_summary_table_of_models,
    command = make_summary_table_of_models_fn(revbayes_script_001_file,
                                              revbayes_script_002_file,
                                              revbayes_script_003_file,
                                              revbayes_script_004_file,
                                              revbayes_script_005_file)
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
