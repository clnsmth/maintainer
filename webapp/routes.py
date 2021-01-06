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
import logging
import os
import socket
import subprocess

import daiquiri
from flask import Flask, request
from sqlalchemy.exc import IntegrityError

from webapp.config import Config
from webapp.ecocomDP_db import EventDb
import webapp.mailout as mailout


cwd = os.path.dirname(os.path.realpath(__file__))
logfile = cwd + "/routes.log"
daiquiri.setup(level=logging.WARNING, outputs=(daiquiri.output.File(logfile), "stdout"))
logger = daiquiri.getLogger(__name__)

app = Flask(__name__)
app.config.from_object(Config)


def get_host(remote: str) -> str:
    prod_addr = socket.gethostbyname(Config.PRODUCTION)
    stage_addr = socket.gethostbyname(Config.STAGING)
    dev_addr = socket.gethostbyname(Config.DEVELOPMENT)
    regan_addr = socket.gethostbyname(Config.REGAN)
    if remote == prod_addr:
        host = Config.PRODUCTION
    elif remote == stage_addr:
        host = Config.STAGING
    elif remote == dev_addr:
        host = Config.DEVELOPMENT
    elif remote in Config.LOCALHOST or remote == regan_addr:
        host = "localhost"
    elif Config.DEBUG:
        host = "localhost"
    else:
        host = None
    return host


def is_valid_package_id(package_id: str) -> bool:
    return len(package_id.split(".")) == 3


@app.route("/ecocom-listener", methods=["POST"])
def insert_event():
    remote = request.remote_addr
    logger.warn(f"Inbound POST from: {remote}")
    host = get_host(remote)
    if host is None:
        msg = f"{host} is not an authorized server"
        logger.warn(msg)
        return msg, http.HTTPStatus.FORBIDDEN
    else:
        package_id = request.data.decode("UTF-8").strip()
        if is_valid_package_id(package_id):
            try:
                EventDb(Config.PATH + Config.DB).insert_event(package_id, host)
            except IntegrityError as ex:
                logger.error(ex)
                msg = f"EventDB integrity error: {package_id} event already recorded"
                mailout.send_mail(msg, msg, Config.MAIL_TO)
                return msg, http.HTTPStatus.BAD_REQUEST
            msg = f"EcocomDP event recorded: {package_id} on {host}"
            logger.warn(msg)
            mailout.send_mail(msg, msg, Config.MAIL_TO)
            cmd = Config.CMD + " " + package_id
            subprocess.Popen(cmd, close_fds=True, shell=True)
            return msg, http.HTTPStatus.OK
        else:
            msg = f"Not valid package identifier: {package_id}"
            logger.warn(msg)
            mailout.send_mail(msg, msg, Config.MAIL_TO)
            return msg, http.HTTPStatus.BAD_REQUEST


@app.route("/ecocom-listener/<env>", methods=["GET"])
def get_next_event(env: str = None):
    remote = request.remote_addr
    logger.warn(f"Inbound GET from: {remote}")
    host = get_host(remote)
    if host is None or host != "localhost":
        msg = f"{host} is not an authorized server"
        logger.warn(msg)
        return msg, http.HTTPStatus.FORBIDDEN
    try:
        index, package_id = EventDb(Config.PATH + Config.DB).get_next_event(env)
    except Exception as ex:
        logger.error(ex)
        msg = f"Failed to get next event from {env}"
        return msg, http.HTTPStatus.NOT_FOUND
    return f"{index},{package_id}", http.HTTPStatus.OK


@app.route("/ecocom-listener/<index>", methods=["DELETE"])
def set_processed(index: str = None):
    remote = request.remote_addr
    logger.warn(f"Inbound DELETE from: {remote}")
    host = get_host(remote)
    if host is None or host != "localhost":
        msg = f"{host} is not an authorized server"
        logger.warn(msg)
        return msg, http.HTTPStatus.FORBIDDEN
    try:
        package_id = EventDb(Config.PATH + Config.DB).set_processed_event(int(index))
    except Exception as ex:
        logger.error(ex)
        msg = f"Failed to remove event with index {index}"
        return msg, http.HTTPStatus.NOT_FOUND
    msg = f"{package_id} set to processed"
    # mailout.send_mail(msg, msg, Config.MAIL_TO)
    return msg, http.HTTPStatus.OK
