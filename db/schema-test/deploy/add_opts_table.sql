-- Deploy avito-schema-test:add_opts_table to pg

BEGIN;

CREATE TABLE opts (
      id            BIGSERIAL     PRIMARY KEY 
    , name         TEXT          UNIQUE
);

COMMIT;
