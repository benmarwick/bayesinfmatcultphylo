# get the base image, the rocker/verse has R, RStudio and pandoc
FROM rocker/verse:4.2.2

# required
MAINTAINER Your Name <your_email@somewhere.com>

COPY . /<REPO>

# go into the repo directory
RUN . /etc/environment \
  # Install linux depedendencies here
  # e.g. need this for ggforce::geom_sina
  && sudo apt-get update \
  && sudo apt-get install libudunits2-dev -y \
  # build this compendium package
  && R -e "install.packages('remotes', repos = c(CRAN = 'https://cloud.r-project.org'))" \
  && R -e "remotes::install_github(c('rstudio/renv', 'quarto-dev/quarto-r'))" \
  # install pkgs we need
  && R -e "renv::restore()" \
  # render the manuscript into a docx, you'll need to edit this if you've
  # customised the location and name of your main qmd file
  && R -e "quarto::quarto_render('/<REPO>/analysis/paper/paper.qmd')"
