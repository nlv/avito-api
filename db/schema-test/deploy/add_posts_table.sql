-- Deploy avito-schema-test:add_posts to pg

BEGIN;

CREATE TABLE posts (
      id            INT           PRIMARY KEY CHECK (id >= 0)
    , oid           VARCHAR(50)   UNIQUE
    , tname         TEXT
    , category      TEXT
    , title         TEXT
    , description   TEXT
    , price         TEXT
    , video_url     TEXT
    , addr_region   TEXT 
    -- , addr_area     TEXT
    , addr_city     TEXT
    , addr_point    TEXT
    , addr_street   TEXT
    , addr_house    TEXT
    , contact_phone TEXT
    , post          JSONB
);

COMMIT;

