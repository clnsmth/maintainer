# webapp

Contains files for the application

`config.py.template` - Template for configuring the listener

`config.txt.template` - Template for configuring the maintainer and user defined workflows

`mailout.py` - Module for sending emails

`maintainer.sqlite` - The queue to which updates are added by the listener

`maintainer_db.py` - Module for transacting with the queue

`routes.py` - Module for handling HTTP requests

`utilities.R` - Utility functions for `workflow_manager.R` and user workflows. See function descriptions in `utilities.R`.

- `compare_eml()` - Identify potentially meaningful differences between EML documents
- `check_series_integrity()` - Check if earlier unprocessed versions of a data package are in the queue
- `get_derived_id()` - Get derived data package identifier from workflow_map.csv
- `get_previous_version()` - Get identifier of previous data package version
- `get_workflows()` - Get name of workflow(s) from workflow_map.csv
- `increment_package_version()` - Increment data package version number
- `msg()` - Add a high-level workflow message to the log file
- `queue_delete()` - Delete record from queue (maintainer.sqlite)
- `queue_get_update()` - Get the next update from the queue (maintainer.sqlite)
- `queue_insert()` - Manually insert a record into queue (maintainer.sqlite)
- `queue_is_empty()` - Check if there are any unprocessed items in the queue (maintainer.sqlite)
- `queue_remove_update()` - Remove an update from the queue (maintainer.sqlite)
- `queue_select_all()` - Return all fields and records in the queue (maintainer.sqlite)
- `queue_update()` - Update a value in the queue (maintainer.sqlite)
- `send_email()` - Send email to a Gmail account
- `send_mock_notification()` - Imitate an event notification sent from the EDI Data Repository
- `with_timeout()` - Time out a function that's taking too long ...

`workflow_manager.R` - Module for handling top level workflow routines. Calls user defined workflows.

`workflow_map.csv` - Maps updates (data package identifiers) to one or more workflows
