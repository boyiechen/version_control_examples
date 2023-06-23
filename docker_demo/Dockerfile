# Use rocker/rstudio:4.3 as base image
FROM rocker/rstudio:4.3

RUN apt-get update && apt-get install -y git
RUN apt-get install -y libgsl-dev
RUN apt-get install -y vim

# Install packages
RUN R -e "install.packages(c('dplyr', 'tidyr', 'readr', 'ggplot2', 'matrixcalc', 'lattice', 'cowplot', 'lubridate', 'xtable', 'assertthat', 'openxlsx', 'mFilter', 'Matrix', 'svars', 'ggfortify', 'renv', 'dotenv'), repos='https://cran.rstudio.com/')"
