#!/bin/bash
# This script will install R and related linux dependencies used by the 
# routine_handler function called by ./webapp/config.py

# Install linux libs required by R libs
sudo apt install libv8-dev
sudo apt install libjq-dev
sudo apt install libgit2-dev
sudo apt install libmariadbclient-dev

# Install R libs
sudo su - -c "R -e \"install.packages('data.table', repos='http://cran.rstudio.com/')\""
sudo su - -c "R -e \"install.packages('downloader', repos='http://cran.rstudio.com/')\""
sudo su - -c "R -e \"install.packages('dplyr', repos='http://cran.rstudio.com/')\""
sudo su - -c "R -e \"install.packages('EML', repos='http://cran.rstudio.com/')\""
sudo su - -c "R -e \"install.packages('gdata', repos='http://cran.rstudio.com/')\""
sudo su - -c "R -e \"install.packages('geosphere', repos='http://cran.rstudio.com/')\""
sudo su - -c "R -e \"install.packages('httr', repos='http://cran.rstudio.com/')\""
sudo su - -c "R -e \"install.packages('jsonlite', repos='http://cran.rstudio.com/')\""
sudo su - -c "R -e \"install.packages('lubridate', repos='http://cran.rstudio.com/')\""
sudo su - -c "R -e \"install.packages('magrittr', repos='http://cran.rstudio.com/')\""
sudo su - -c "R -e \"install.packages('neonUtilities', repos='http://cran.rstudio.com/')\""
sudo su - -c "R -e \"install.packages('purrr', repos='http://cran.rstudio.com/')\""
sudo su - -c "R -e \"install.packages('reader', repos='http://cran.rstudio.com/')\""
sudo su - -c "R -e \"install.packages('readr', repos='http://cran.rstudio.com/')\""
sudo su - -c "R -e \"install.packages('RMySQL', repos='http://cran.rstudio.com/')\""
sudo su - -c "R -e \"install.packages('stringi', repos='http://cran.rstudio.com/')\""
sudo su - -c "R -e \"install.packages('stringr', repos='http://cran.rstudio.com/')\""
sudo su - -c "R -e \"install.packages('tidyr', repos='http://cran.rstudio.com/')\""
sudo su - -c "R -e \"install.packages('tidyverse', repos='http://cran.rstudio.com/')\""
sudo su - -c "R -e \"install.packages('usethis', repos='http://cran.rstudio.com/')\""
sudo su - -c "R -e \"install.packages('xml2', repos='http://cran.rstudio.com/')\""
sudo su - -c "R -e \"install.packages('XML', repos='http://cran.rstudio.com/')\""
sudo su - -c "R -e \"install.packages('shiny', repos='http://cran.rstudio.com/')\""
sudo su - -c "R -e \"install.packages('plotly', repos='http://cran.rstudio.com/')\""
sudo su - -c "R -e \"install.packages('shinyWidgets', repos='http://cran.rstudio.com/')\""
sudo su - -c "R -e \"install.packages('shinythemes', repos='http://cran.rstudio.com/')\""
sudo su - -c "R -e \"install.packages('remotes', repos='http://cran.rstudio.com/')\""
sudo su - -c "R -e \"remotes::install_github('EDIorg/EDIutils')\""
sudo su - -c "R -e \"remotes::install_github('EDIorg/taxonomyCleanr')\""
sudo su - -c "R -e \"remotes::install_github('EDIorg/dataCleanr')\""
sudo su - -c "R -e \"remotes::install_github('EDIorg/ecocomDP')\""

