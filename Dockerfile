FROM rocker/geospatial:4.2.2

# copy our project into the directory that RStudio opens at
COPY . /home/rstudio/bayesinfmatcultphylo

RUN  . /etc/environment \
  # set permissions for read and write, we need this to write output files
  && sudo chmod -R 777 /home/ \
  && apt-get update \
  && apt-get install -y  imagemagick libmagickwand-dev --no-install-recommends \
  # install pkgs we need for the analysis
 && R -e "install.packages('remotes', repos = c(CRAN = 'https://cloud.r-project.org'))" \
 && R -e "remotes::install_github(c('rstudio/renv', 'quarto-dev/quarto-r'))" \
 && R -e "setwd('/home/rstudio/bayesinfmatcultphylo'); renv::restore()" \
  # Download and unzip RevBayes into the RStudio default working directory
  && cd /home/rstudio/ && wget https://github.com/revbayes/revbayes/releases/download/v1.2.1/revbayes-v1.2.1-linux64.tar.gz && tar -xvzf revbayes-v1.2.1-linux64.tar.gz

# notes on how to make this work:
# step 1 ----------------------------------------------------------------
# docker build -t test  -f 'Dockerfile' .
# step 2 ----------------------------------------------------------------
# docker run --rm -it -e ROOT=TRUE -e PASSWORD=rstudio -dp 8787:8787 test
# step 3 clean up by stopping and deleting all containers ---------------
# docker ps -aq | xargs docker stop | xargs docker rm


