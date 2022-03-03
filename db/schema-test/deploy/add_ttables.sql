-- Deploy avito-schema-test:add_ttables to pg

BEGIN;

CREATE TABLE ttables (
      id            BIGSERIAL     PRIMARY KEY 
    , tname         TEXT          NOT NULL REFERENCES tnames (tname)
    , name          TEXT          NOT NULL UNIQUE
);

COMMIT;
