# maintainer

Listens for updates to data packages in the EDI repository and runs user defined workflows.

## Overview

The event notification service of the EDI Data Repository allows users to subscribe to data packages of interest and receive a notification when a subscription updates, providing a basis for workflow automation. `maintainer` is a web service endpoint for collecting notifications and a workflow processor according to user specified routines.

EDI notifications are passed as HTTP requests to the Nginx web server which is a reverse proxy to uWSGI serving the Python Flask web application that parses and validates request, stores the notification in an SQLite database queue, and runs a user defined command, typically a high-level script controlling workflow execution. Below is an overview of this process.

FIGURE - Sequence diagram

## Getting Started
* Fork the `maintainer` repository
* Deploy to web server
* Subscribe to notifications
* Customize workflows

## Getting Help 
* FAQ
* Open an issue
* Contact us (info@environmentaldatainitiative.org)

## News
Updates are listed in [News](https://github.com/clnsmth/maintainer/blob/master/NEWS.md)

## Road Map
* Docker - Create a Docker instance to ease deployment

## Contributing
Please contribute! See our [code of conduct](https://github.com/clnsmth/maintainer/blob/master/CODE_OF_CONDUCT.md) and [contributing guidelines](https://github.com/clnsmth/maintainer/blob/master/CONTRIBUTING.md) for details.
