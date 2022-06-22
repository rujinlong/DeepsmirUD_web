FROM --platform=linux/amd64 rocker/tidyverse:4.2.0

LABEL author="Jinlong Ru"

RUN R -e 'install.packages(c("remotes", "argparser"), repos="https://cloud.r-project.org/")'
RUN R -e 'remotes::install_github("Jasonlinchina/RCSM")'
