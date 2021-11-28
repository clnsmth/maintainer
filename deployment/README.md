# deployment

Contains files to install the maintainer application.

`maintainer.nginx` - The Nginx web server configuration.

`maintainer.service` - Tells the system how to start up uWSGI. It allows uWSGI and the Python Flask application (the listener) to start up at boot time.

`maintainer.ini` - The uWSGI configuration.
