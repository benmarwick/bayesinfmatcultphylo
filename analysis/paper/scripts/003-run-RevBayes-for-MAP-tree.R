# run RevBayes file to set up and run model, output is exported as files
# to data directory. I'm using RevBayes v1.2.1, the binary executable is located
# on my computer at /Applications/revbayes-v1.2.1/bin/rb
# The working directory for terminal commands is this RStudio Project

# Start a command with results displayed in a terminal buffer
# do tree inference, make a file path to the script to execute
run_revbayes_script_01_fn <- function(file,
                                      data_for_revbayes) {
  x <- data_for_revbayes
  system(paste0("cd ",
                here::here(), # change to project working directory
                " && ",
                "/Applications/revbayes-v1.2.1/bin/rb  ",
                file # this is the .Rev file path we set in _targets.R
                ))
}

run_revbayes_script_02_fn <- function(file,
                                      data_for_revbayes) {
  x <- data_for_revbayes
  system(paste0("cd ",
                here::here(), # change to project working directory
                " && ",
                "/Applications/revbayes-v1.2.1/bin/rb  ",
                file # this is the .Rev file path we set in _targets.R
  ))
}

run_revbayes_script_03_fn <- function(file,
                                      data_for_revbayes) {
  x <- data_for_revbayes
  system(paste0("cd ",
                here::here(), # change to project working directory
                " && ",
                "/Applications/revbayes-v1.2.1/bin/rb  ",
                file # this is the .Rev file path we set in _targets.R
  ))
}

run_revbayes_script_04_fn <- function(file,
                                      data_for_revbayes) {
  x <- data_for_revbayes
  system(paste0("cd ",
                here::here(), # change to project working directory
                " && ",
                "/Applications/revbayes-v1.2.1/bin/rb  ",
                file # this is the .Rev file path we set in _targets.R
  ))
}

run_revbayes_script_05_fn <- function(file,
                                      data_for_revbayes) {
  x <- data_for_revbayes
  system(paste0("cd ",
                here::here(), # change to project working directory
                " && ",
                "/Applications/revbayes-v1.2.1/bin/rb  ",
                file # this is the .Rev file path we set in _targets.R
  ))
}






