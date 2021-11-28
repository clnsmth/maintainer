# Add Workflows

## Overview

Each time an event notification is received the workflow manager (`webapp/workflow_manager.R`) sources the `workflows/` directory (containing user defined workflows and configurations), initializes session logging and other high level tasks, then begins iterating through each unprocessed updates in the queue (`webapp/maintainer.sqlite`) beginning with the oldest. The data package identifier of the update is then used to find the associated workflow name, and finally `run_workflow()` is called to execute the corresponding workflow code.

## Step-by-Step

1. Add an entry to `webapp/workflow_map.csv`

- In the "environment" field, add the EDI repository tier ("production", "staging", or "development") in which the event subscription was made.

- In the "source_id" field, add the identifier of the data package, data package series, or scope you've subscribed to. This is identifies the source data to the workflow.

- In the "derived_id" field, add the identifier of any derived data package to be published as a result of the workflow. Leave this field empty if no derived data package will be created.

- In the "workflow" field, add the name of the workflow to execute. This workflow name should match the if/else logic in the `workflows/run_workflow.R` module. If more than one workflow should be executed per source_id, simply create a new record repeating the "environment", "source_id" with different "derived_id" and "workflow" values.

2. Add global configuration to `workflows/config.txt`

Create a copy of `webapp/config.txt.template` to `workflows/config.txt` and add relevant variables and libraries to be used during workflow execution. The template contains some commonly used variables, but can be customized.

3. Add the workflow code to `workflows/`

- Add the workflow code to the `workflows/` directory. This code should take the form of one or more functions rather than scripts, since the `workflows/` directory is sourced when the workflow manager starts up.

4. Add flow control logic to `workflows/run_workflow.R`

- Add an `if` statement to call the correct workflow. The workflow name must match the value listed in the "workflow" column of `webapp/workflow_map.csv` otherwise it will not be executed.


