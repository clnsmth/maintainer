#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""
:Mod: test_ecocomDP_db

:Synopsis:

:Author:
    servilla

:Created:
    11/6/20
"""
from pathlib import Path

import pytest

from webapp.config import Config
from webapp.ecocomDP_db import EventDb


events = [
    ("edi.213.1", Config.PRODUCTION),
    ("edi.213.2", Config.PRODUCTION),
    ("edi.3.32", Config.STAGING),
    ("edi.213.3", Config.PRODUCTION),
    ("edi.5.23", Config.PRODUCTION),
    ("edi.34.2", Config.STAGING),
]

db_path = Config.TEST_PATH + Config.DB


@pytest.fixture()
def e_db():
    return EventDb(db_path)


@pytest.fixture()
def clean_up():
    yield
    Path(db_path).unlink(missing_ok=True)


def test_event_db_connection(e_db, clean_up):
    pass


def test_insert_events(e_db, clean_up):
    cnt = 0
    for event in events:
        cnt += 1
        index = e_db.insert_event(event[0], event[1])
        assert index == cnt


def test_get_next_event(e_db, clean_up):
    for event in events:
        index = e_db.insert_event(event[0], event[1])

    index, pid = e_db.get_next_event(Config.STAGING)
    assert index == 3
    assert pid == events[2][0]


def test_set_processed_event(e_db, clean_up):
    for event in events:
        index = e_db.insert_event(event[0], event[1])

    index, pid, env, processed = e_db.get_event(3)
    assert not processed

    e_db.set_processed_event(3)

    index, pid, env, processed = e_db.get_event(3)
    assert processed

    index, pid = e_db.get_next_event(Config.STAGING)
    assert index == 6
    assert pid == events[5][0]
