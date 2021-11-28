# Deployment

There are two parts of the deployment. First, setting up the Nginx web server to point back to something Nginx can forward a request to, and to listen for a response and turn that response back to the client. Second, which is a little more involved, is set up of the system control file.

Below is an example deployment on Linux Ubuntu 18.04.6 with an Nginx web server for the user `pasta`. Other top level dependencies are listed in `environment-min.yml`. This deployment assumes there is an SSL certificate setup by Let's Encrypt and the deploying person has root user access.

### 1. Login and Clone

Login to the server and sudo to the user under which `maintainer` will be deployed.

```
sudo su -l pasta
```

Clone your fork of the `maintainer` app to your home directory

```
cd /home/pasta
git clone https://github.com/clnsmth/maintainer.git
cd maintainer
```

### 2. Configure Nginx

Configure the Nginx web server for `maintainer`.

```
vi deployment/maintainer.nginx

# Display line numbers
:se nu
```

Line 3 - Replace "regan.edirepository.org" with your server name.

```
# Save and exit
:wq
```

### 3. Configure `maintainer` to Start Up at Boot Time

```
vi deployment/maintainer.service
```

Line 6 - Defines who this service should run under. Replace "pasta" with your user name.

Line 8 - This is the `maintainer` working directory. Replace "pasta" with your user name.

Line 9 - Path to Python environment that will be running the `maintainer` service. In this example anaconda3 is installed in the home directory. Replace "pasta" with your user name.

Line 10 - Tells the system how to start this service. Note: This references the initialization file. Replace "pasta" with your user name.

### 4. Configure uWSGI

```
vi deployment/maintainer.ini
```

Line 7 - Replace "pasta" with your user name.

### 5. Create the Virtual Environment

Create the virtual python environment in which `maintainer` will run and which is populated by dependencies listed in `environment.yml`.

```
conda env create --file environment.yml
conda activate maintainer 
```

### 6. Configure the Python Listener

```
cd webapp
```

Create a copy of the template and configure.

```
cp config.py.template /config.py
vi config.py
```

Line 20 - Add your secret key.

Line 21 - "True" allows mocked POST requests to activate the maintainer, which is helpful when testing during the deployment phase. Set to "False" once successfully deployed and not debugging.

Line 23 - Modify path to the webapp. Replace "pasta" with your user name.

Line 24 - Modify path to tests. Replace "pasta" with your user name.

Line 26 - This is the command executed each time the listener receives an update. You may want to modify this command in it's entirety. But if using the default workflow manager, then replace "pasta" with your user name.

Line 33 - Replace "regan.edirepository.org" with your server name.

Lines 42-44 - Credentials for receiving an email each time an update occurs. The email contains a subject line and body with the updated data package identifier. Only Gmail is currently supported and requires “Less secure app access” to your account. [See here for more details](https://support.google.com/accounts/answer/6010255?hl=en). _Be sure to comment out these lines if not using this feature, otherwise an faulty configuration will prevent the maintainer app from executing._

### 7. Setup system d

Must be root user to do this.

```
sudo su -l
cd /etc/systemd/system/
```

Copy the `maintainer.service` file to this location.

```
cp /home/pasta/maintainer/deployment/maintainer.service .
```

Start it up to create the socket file

```
 systemctl start maintainer.service
```

Stop the Nginx file, then edit.

```
systemctl stop maintainer.service
vi /etc/nginx/sites-enabled/regan-ssl # Replace "regan" with your server name.
```

Add this code before ssl certificate block:

```
location /maintainer {
              include uwsgi_params;
              uwsgi_pass unix:///tmp/maintainer.sock;
}
```

Test the Nginx configuration.

```
nginx -t
```

### 8. Enable `maintainer`

This installs the start up script so when the system starts it will boot up. If you don't do this, then you will have to manually boot it up whenever you want to access it.

```
systemctl enable maintainer.service
systemctl start maintainer.service
systemctl status maintainer.service
```

### 9. Restart Nginx

In order for changes to be picked up, we need to restart.

```
systemctl restart nginx.service
```

### 10. Make `temp/` Web Accessible

If you will be publishing to a data repository, then you will need to make a web accessible directory from which files can be downloaded. Must be logged in as root user.

```
cd /var/www/html
ln -s /home/pasta/maintainer temp # Replace "pasta" with your user name
```

### 11. Restart `maintainer` and Logout

Anytime changes are made to `maintainer` files in `deployment/` or `webapp/`, you'll need to restart the maintainer.service, otherwise the changes will not apply.

```
systemctl restart maintainer.service
logout # Once for root user
logout # And again for you
```

### 12. Testing

There are a few ways to check if the deployment was successful:

1. In a web browser, enter the address of your web service endpoint (e.g. https://regan.edirepository.org/maintainer). A successful deployment will return a message stating "Method Not Allowed". The web browser is performing an HTTP GET request and the end point only accepts POST requests, thus the "Method Not Allowed" message.

2. Send an imitated event notification from a local R session with the utility function `send_mock_notification()` 

```
source("workflows/utilities.R")
send_mock_notification("https://my.repository.org/maintainer")
```

Then check for an expected response. A couple places to look are:

- An email notification listing the data package identifier. This will only work if your `webapp/config.py` is configured to send emails, and if the corresponding account accepts access by less secure apps.

- Creation of a new log file in `logs/`.
