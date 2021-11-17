# This script is called by the listener whenever an update to a subscribed data
# package occurs

# Source workflow configuration variables and functions
scripts <- list.files("/home/pasta/maintainer/workflows", full.names = TRUE)
invisible(sapply(scripts, source))

# Call the top level function that manages all workflows
workflow_manager()
