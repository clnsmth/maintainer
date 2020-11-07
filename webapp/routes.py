#!/usr/bin/env python
# -*- coding: utf-8 -*-

""":Mod: routes

:Synopsis:

:Author:
    servilla

:Created:
    5/15/19
"""
import http
import socket
import subprocess

import daiquiri
from flask import Flask, request
from sqlalchemy.exc import IntegrityError

from webapp.config import Config
from webapp.ecocomDP_db import EventDb
import webapp.mailout as mailout


logger = daiquiri.getLogger("routes: " + __name__)

app = Flask(__name__)
app.config.from_object(Config)


def get_pasta_host(remote: str) -> str:
    prod_addr = socket.gethostbyname(Config.PRODUCTION)
    stage_addr = socket.gethostbyname(Config.STAGING)
    dev_addr = socket.gethostbyname(Config.DEVELOPMENT)
    if remote == prod_addr:
        host = Config.PRODUCTION
    elif remote == stage_addr:
        host = Config.STAGING
    elif remote == dev_addr:
        host = Config.DEVELOPMENT
    elif Config.DEBUG:
        host = "any_host"
    else:
        host = None
    return host


def is_valid_package_id(package_id: str) -> bool:
    return len(package_id.split(".")) == 3


@app.route("/ecocom-listener", methods=["POST"])
def ecocom_listener():
    r = request
    remote = request.remote_addr
    host = get_pasta_host(remote)
    if host is None:
        return "Not authorized server", http.HTTPStatus.FORBIDDEN
    else:
        package_id = request.data.decode("UTF-8").strip()
        if is_valid_package_id(package_id):
            try:
                EventDb(Config.PATH + Config.DB).insert_event(package_id, host)
            except IntegrityError as ex:
                logger.error(ex)
                msg = f"EcocomDP database integrity error for: {package_id}"
                mailout.send_mail(msg, msg, Config.MAIL_TO)
                return msg, http.HTTPStatus.BAD_REQUEST
            msg = (
                f"EcocomDP event for package {package_id} on {host} recorded in EventDB"
            )
            mailout.send_mail(msg, msg, Config.MAIL_TO)
            cmd = Config.CMD + " " + package_id
            subprocess.Popen(cmd, close_fds=True, shell=True)
            return f"{package_id}\n", http.HTTPStatus.OK
        else:
            return (
                f"Not valid package identifier: {package_id}",
                http.HTTPStatus.BAD_REQUEST,
            )


if __name__ == "__main__":
    app.run()
