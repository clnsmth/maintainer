# webapp

Contains files for the application

`config.py.template` - Template for configuring the listener

`config.txt.template` - Template for configuring workflows

`mailout.py` - Module for sending emails

`maintainer.sqlite` - The queue to which updates are added by the listener

`maintainer_db.py` - Module for transacting with the queue

`routes.py` - Module for handling HTTP requests

`workflow_manager.R` - Module for handling top level workflow routines. Calls user defined workflows.

`workflow_map.csv` - Maps updates (data package identifiers) to one or more workflows
