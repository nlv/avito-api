-- Deploy avito-schema-test:add_for_house to pg

BEGIN;

CREATE TABLE for_house (
      id            INT           PRIMARY KEY CHECK (id >= 0)
    , oid           VARCHAR(50)   UNIQUE
    , category      TEXT
    , goods_type    TEXT
    , title         TEXT
    , description   TEXT
    , price         TEXT
    , image_names   TEXT
    , video_url     TEXT
    , addr_region   TEXT 
    -- , addr_area     TEXT
    , addr_city     TEXT
    , addr_point    TEXT
    , addr_street   TEXT
    , addr_house    TEXT
    , contact_phone TEXT
);

COMMIT;

