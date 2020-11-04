#!/usr/bin/env python
# -*- coding: utf-8 -*-

""":Mod: routes

:Synopsis:

:Author:
    servilla

:Created:
    5/15/19
"""
import daiquiri
from flask import Flask, make_response, redirect, render_template, request, url_for

from webapp.config import Config


logger = daiquiri.getLogger("routes: " + __name__)

app = Flask(__name__)
app.config.from_object(Config)


@app.route("/ecocom-listener", methods=["POST"])
def ecocom_listener():
    return "Hello on wheels"


if __name__ == "__main__":
    app.run(ssl_context="adhoc")
