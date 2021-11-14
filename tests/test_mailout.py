#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""
:Mod: test_mailout

:Synopsis:

:Author:
    servilla

:Created:
    11/6/20
"""
import logging
import os

import daiquiri

import webapp.mailout as mailout

cwd = os.path.dirname(os.path.realpath(__file__))
logfile = cwd + "/test_mailout.log"
daiquiri.setup(level=logging.INFO, outputs=(daiquiri.output.File(logfile), "stdout"))
logger = daiquiri.getLogger(__name__)


def test_send_mail():
    subject = "Test maintainer notification email notification..."
    msg = "IGNORE -- Test maintainer notification email notification."
    to = "mark.servilla@gmail.com"
    mailout.send_mail(subject, msg, to)
