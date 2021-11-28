# Subscribe to Notifications

To create event subscriptions:

## 1. Login to the EDI Data Portal

There are three tiers to choose from:

- [Production](https://portal.edirepository.org/nis/home.jsp), where data are officially published.

- [Staging](https://portal-s.edirepository.org/nis/home.jsp), a sandbox environment for testing workflows and the rendering of data publications. Data here are not "published".

- [Development](https://portal-d.edirepository.org/nis/home.jsp), where new repository features are tested. This environment is similar to "Staging", but less stable.

Login to the tier containing the data package you want a subscription to. _NOTE: The three repository tiers are completely independent of each other. A data package with identifier "edi.100.1" in production may be completely different than "edi.100.1" in staging._

## 2. Create a subscription

Navigate to the subscription page (TOOLS > Event Subscriptions).

Add the data package of interest to the "Package Id" field. There are three options:

- Subscribe to a specific data package revision, e.g. "edi.100.1". Doing this will send a notification only once, when the specific version "edi.100.1" is updated, e.g. to "edi.100.2".

- Subscribe to a data package series, e.g. "edi.100". This will send a notification each time a new revision is added to the "edi.100" series, i.e. a notification will be sent when "edi.100.1" updates to "edi.100.2", when "edi.100.2" updates to "edi.100.3", etc.

- Subscribe to a data package scope, e.g. "edi" or "knb-lter-niw". This will send a notification each time a new data package series or revision is created within the specified scope.

Add the URL of your maintainer web service endpoint to the "Target URL" field (e.g. https://my.repository.org/maintainer). 

Regardless of the type of subscription specified in the "Package Id" field, the notification will be an HTTP POST request containing the most recent data package identifier in the form: scope.identifier.revision.

