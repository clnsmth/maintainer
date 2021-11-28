# maintainer

Listens for updates to data packages in the EDI repository and runs user defined workflows.

## Overview

The event notification service of the EDI Data Repository allows users to subscribe to data packages of interest and receive a notification when a subscription updates, providing a basis for workflow automation. `maintainer` is a web service endpoint for collecting notifications and a workflow processor according to user specified routines.

EDI notifications are passed as HTTP requests to the Nginx web server which is a reverse proxy to uWSGI serving the Python Flask web application that parses and validates request, stores the notification in an SQLite database queue, and runs a user defined command, typically a high-level script controlling workflow execution. Below is an overview of this process.

FIGURE - Sequence diagram

## Getting Started
* Fork and clone the `maintainer` repository
* [Add your data processing workflows](https://github.com/clnsmth/maintainer/blob/main/docs/add_workflows.md)
* [Deploy to a web server](https://github.com/clnsmth/maintainer/blob/main/docs/deployment.md)
* [Subscribe to notifications](https://github.com/clnsmth/maintainer/blob/main/docs/subscribe.md)

## Getting Help 
* FAQ
* Open an issue
* Contact us (info@environmentaldatainitiative.org)

## News
Updates are listed in [News](https://github.com/clnsmth/maintainer/blob/master/NEWS.md)
