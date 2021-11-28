# maintainer

Listens for updates to data packages in the EDI repository and runs user defined workflows.

## Overview

The event notification service of the EDI Data Repository allows users to subscribe to data packages of interest and receive notification when subscriptions update, thereby providing a basis for workflow automation. `maintainer` is a web service for collecting notifications and processing them according to user specified routines.

In this example implementation a EDI event notifications are sent (as HTTP POST requests) to an Nginx web server, relayed through uWSGI to a Python Flask framework, which in turn logs the update to a processing queue and calls a workflow manager that executes the corresponding user defined routine in R. Below is an overview of this process.

FIGURE - Sequence diagram

## Getting Started
* Fork and clone the `maintainer` repository
* [Add Workflows](https://github.com/clnsmth/maintainer/blob/main/docs/add_workflows.md)
* [Deploy to Web Server](https://github.com/clnsmth/maintainer/blob/main/docs/deployment.md)
* [Subscribe to Notifications](https://github.com/clnsmth/maintainer/blob/main/docs/subscribe.md)

## Getting Help 
* [Open an issue]()
* Contact us at info@environmentaldatainitiative.org

## News
Project updates are listed in [NEWS](https://github.com/clnsmth/maintainer/blob/master/NEWS.md)
