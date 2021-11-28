# workflows
Contains user defined workflow configurations and modules. This directory is sourced by workflow_manager.R each time an update is received.

`run_workflow.R` - Module containing `run_workflow()`, the flow control logic executing workflows by names listed in `webapp/workflow_map.csv`. This is the function that is called by `workflow_manager.R` each time an update is received.

`update_ecocomDP.R` - An example workflow for converting a data package to the ecocomDP format.

`update_DwCA.R` - An example workflow for converting an ecocomDP formatted data package to a Darwin Core Event Core Archive.
