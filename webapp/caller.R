# This script is called by the listener when an update to a subscription occurs

# Load configuration variables
source("./ecocomDP-maintainer/webapp/config_workflows.txt") # TODO convert to relative and stable path

# Source functions used by the routine handler
scripts <- list.files("./ecocomDP-maintainer/workflows", full.names = TRUE)
invisible(sapply(scripts, source))

# Call the routine handler
routine_handler()