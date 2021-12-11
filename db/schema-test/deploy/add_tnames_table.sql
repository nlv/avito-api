-- Deploy avito-schema-test:add_tnames_table to pg

BEGIN;

CREATE TABLE tnames (
      id            BIGSERIAL     PRIMARY KEY 
    , tname         TEXT          UNIQUE
    , label         TEXT
);

COMMIT;
