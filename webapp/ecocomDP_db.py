#!/usr/bin/env python
# -*- coding: utf-8 -*-

"""
:Mod: ecocomDP_db

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


logger = daiquiri.getLogger("ecocomDP_db: " + __name__)

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
            logger.error(ex)
            self.session.rollback()
            raise ex
        return index

    def get_event(self, index: int) -> Tuple:
        try:
            query = self.session.query(Event)
            e = query.filter(Event.index == index).one()
            return e.index, e.pid, e.env, e.processed
        except IntegrityError as ex:
            logger.error(ex)
            raise ex

    def get_next_event(self, env: str) -> Tuple:
        try:
            query = self.session.query(Event)
            e = (
                query.filter(Event.env == env, Event.processed.is_(False))
                .order_by(Event.dt.asc())
                .first()
            )
            return e.index, e.pid
        except IntegrityError as ex:
            logger.error(ex)
            raise ex

    def set_processed_event(self, index: int):
        try:
            query = self.session.query(Event)
            e = query.filter(Event.index == index).one()
            e.processed = True
            self.session.commit()
        except IntegrityError as ex:
            logger.error(ex)
            self.session.rollback()
            raise ex
