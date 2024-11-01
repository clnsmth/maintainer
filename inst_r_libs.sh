#!/bin/bash
# This script will install R and related linux dependencies used by the 
# workflow_manager() function called by ./webapp/config.py

# Install linux libs required by R libs
sudo apt install libv8-dev
sudo apt install libjq-dev
sudo apt install libgit2-dev
sudo apt install libmariadbclient-dev

# Install R libs
sudo su - -c "R -e \"install.packages('data.table', repos='http://cran.rstudio.com/')\""
sudo su - -c "R -e \"install.packages('dplyr', repos='http://cran.rstudio.com/')\""
sudo su - -c "R -e \"install.packages('ecocomDP', repos='http://cran.rstudio.com/')\""
sudo su - -c "R -e \"install.packages('EML', repos='http://cran.rstudio.com/')\""
sudo su - -c "R -e \"install.packages('ggplot2', repos='http://cran.rstudio.com/')\""
sudo su - -c "R -e \"install.packages('git2r', repos='http://cran.rstudio.com/')\""
sudo su - -c "R -e \"install.packages('httr', repos='http://cran.rstudio.com/')\""
sudo su - -c "R -e \"install.packages('lubridate', repos='http://cran.rstudio.com/')\""
sudo su - -c "R -e \"install.packages('magrittr', repos='http://cran.rstudio.com/')\""
sudo su - -c "R -e \"install.packages('readr', repos='http://cran.rstudio.com/')\""
sudo su - -c "R -e \"install.packages('remotes', repos='http://cran.rstudio.com/')\""
sudo su - -c "R -e \"install.packages('RSQLite', repos='http://cran.rstudio.com/')\""
sudo su - -c "R -e \"install.packages('tidyr', repos='http://cran.rstudio.com/')\""
sudo su - -c "R -e \"install.packages('tidyverse', repos='http://cran.rstudio.com/')\""
sudo su - -c "R -e \"install.packages('stringr', repos='http://cran.rstudio.com/')\""
sudo su - -c "R -e \"install.packages('xml2', repos='http://cran.rstudio.com/')\""
sudo su - -c "R -e \"remotes::install_github('EDIorg/EDIutils')\""
sudo su - -c "R -e \"remotes::install_github('EDIorg/taxonomyCleanr')\""