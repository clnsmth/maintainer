#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""
:Mod: maintainer_db

:Synopsis:

:Author:
    servilla

:Created:
    11/6/20
"""
from datetime import datetime
from typing import Tuple

import daiquiri
from sqlalchemy import (
    Column,
    String,
    DateTime,
    Boolean,
    Integer,
    create_engine,
    UniqueConstraint,
)
from sqlalchemy.exc import IntegrityError
from sqlalchemy.ext.declarative import declarative_base
from sqlalchemy.orm import sessionmaker
from sqlalchemy.orm.query import Query


logger = daiquiri.getLogger("maintainer_db: " + __name__)

Base = declarative_base()


class Event(Base):
    __tablename__ = "events"

    index = Column(Integer(), primary_key=True)
    pid = Column(String(), nullable=False)
    env = Column(String(), nullable=False)
    dt = Column(DateTime(), nullable=False, default=datetime.now)
    processed = Column(Boolean(), nullable=False, default=False)

    __table_args__ = (UniqueConstraint("pid", "env"),)


class EventDb:
    def __init__(self, db: str):
        engine = create_engine(
            "sqlite:///" + db, connect_args={"check_same_thread": False}
        )
        Base.metadata.create_all(engine)
        Session = sessionmaker(bind=engine)
        self.session = Session()

    def insert_event(self, pid: str, env: str) -> int:
        e = Event(pid=pid, env=env)
        try:
            self.session.add(e)
            self.session.commit()
            index = e.index
        except IntegrityError as ex:
            self.session.rollback()
            raise ex
        return index

    def get_all_unprocessed_events(self, env: str) -> Query:
        try:
            query = self.session.query(Event)
            q = query.filter(Event.env == env, Event.processed.is_(False)).order_by(
                Event.dt.asc()
            )
            return q
        except IntegrityError as ex:
            raise ex

    def get_event(self, index: int) -> Tuple:
        try:
            query = self.session.query(Event)
            e = query.filter(Event.index == index).one()
            return e.index, e.pid, e.env, e.processed
        except IntegrityError as ex:
            raise ex

    def get_next_event(self, env: str) -> Tuple:
        query = self.session.query(Event)
        e = (
            query.filter(Event.env == env, Event.processed.is_(False))
            .order_by(Event.dt.asc())
            .first()
        )
        if e is None:
            raise ValueError("Event not found in EventDB")
        return e.index, e.pid

    def set_processed_event(self, index: int):
        query = self.session.query(Event)
        e = query.filter(Event.index == index).one()
        if e is None:
            raise ValueError("Event not found in EventDB")
        e.processed = True
        self.session.commit()
        return e.pid
