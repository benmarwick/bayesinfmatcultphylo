# run RevBayes file to set up and run model, output is exported as files
# to data directory. I'm using RevBayes v1.2.1, the binary executable is located
# on my computer at /Applications/revbayes-v1.2.1/bin/rb
# The working directory for terminal commands is this RStudio Project

# Start a command with results displayed in a terminal buffer
# do tree inference, make a file path to the script to execute

if_no_output_files_then_run_revbayes_scripts_fn <- function(){

  # check to see what folders of output we have
  library(tidyverse)
  library(fs)
  revbayes_script_files =
    dir_ls(here::here("analysis/paper/scripts")) %>%
    str_subset("\\.Rev")
  output_data_files =
    dir_ls(here::here("analysis/data/derived_data")) %>%
    str_subset("output-0")

  # make a data frame of folder and file names
  # https://stackoverflow.com/questions/49346420/data-table-avoid-recycling
  n <- max(length(revbayes_script_files),
           length(output_data_files))
  scripts_and_outputs_tbl <-
  tibble(revbayes_script_files,
         output_data_files = output_data_files[1:n])

  # if we have a Rev script that doesn't have a corresponding
  # output folder, then run the target for that script
  for (i in 1:length(scripts_and_outputs_tbl$output_data_files)) {

    if(is.na(scripts_and_outputs_tbl$output_data_files[i])) {
      num <-  substr(basename(scripts_and_outputs_tbl$revbayes_script_files[i]), 5, 7)
      target_to_run <- paste0("run_revbayes_script_", num)
      file <-  here::here(paste0("analysis/paper/scripts/004-", num, "-RevBayes-for-MAP.Rev"))
      system(
        paste0("cd ",
               here::here(), # change to project working directory
               " && ",
               "/Applications/revbayes-v1.2.1/bin/rb  ",
               file)
        )
    }

  }

}


run_revbayes_script_001_fn <- function(file,
                                      data_for_revbayes) {
  x <- data_for_revbayes
  system(paste0("cd ",
                here::here(), # change to project working directory
                " && ",
                "/Applications/revbayes-v1.2.1/bin/rb  ",
                file # this is the .Rev file path we set in _targets.R
                ))
}

run_revbayes_script_002_fn <- function(file,
                                      data_for_revbayes) {
  x <- data_for_revbayes
  system(paste0("cd ",
                here::here(), # change to project working directory
                " && ",
                "/Applications/revbayes-v1.2.1/bin/rb  ",
                file # this is the .Rev file path we set in _targets.R
  ))
}

run_revbayes_script_003_fn <- function(file,
                                      data_for_revbayes) {
  x <- data_for_revbayes
  system(paste0("cd ",
                here::here(), # change to project working directory
                " && ",
                "/Applications/revbayes-v1.2.1/bin/rb  ",
                file # this is the .Rev file path we set in _targets.R
  ))
}

run_revbayes_script_004_fn <- function(file,
                                      data_for_revbayes) {
  x <- data_for_revbayes
  system(paste0("cd ",
                here::here(), # change to project working directory
                " && ",
                "/Applications/revbayes-v1.2.1/bin/rb  ",
                file # this is the .Rev file path we set in _targets.R
  ))
}

run_revbayes_script_005_fn <- function(file,
                                      data_for_revbayes) {
  x <- data_for_revbayes
  system(paste0("cd ",
                here::here(), # change to project working directory
                " && ",
                "/Applications/revbayes-v1.2.1/bin/rb  ",
                file # this is the .Rev file path we set in _targets.R
  ))
}






