#!/usr/bin/env python
# -*- coding: utf-8 -*-

""":Mod: mailout

:Synopsis:

:Author:
    servilla

:Created:
    11/6/20
"""
import smtplib

import daiquiri

from webapp.config import Config


logger = daiquiri.getLogger("mailout: " + __name__)


def send_mail(subject: str = None, msg: str = None, to: str = None) -> str:

    # Convert subject and msg to byte array
    body = (
        ("Subject: " + subject + "\n").encode()
        + ("To: " + to + "\n").encode()
        + ("From: " + Config.GMAIL_NAME + "\n\n").encode()
        + (msg + "\n").encode()
    )

    smtpObj = smtplib.SMTP("smtp.gmail.com", 587)
    try:
        smtpObj.ehlo()
        smtpObj.starttls()
        smtpObj.login(Config.GMAIL_NAME, Config.GMAIL_PASSWORD)
        smtpObj.sendmail(from_addr=Config.GMAIL_NAME, to_addrs=to, msg=body)
        response = "Sending email to " + to + " succeeded"
        logger.info(response)
        return response
    except Exception as e:
        response = "Sending email failed - " + str(e)
        logger.error(response)
        raise ConnectionError(response)
    finally:
        smtpObj.quit()
