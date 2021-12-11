-- Deploy avito-schema-test:add_opts_vals_table to pg
-- requires: add_opts_table

BEGIN;

CREATE TABLE opts_vals (
      id            BIGSERIAL     PRIMARY KEY 
    , optname       TEXT          NOT NULL REFERENCES opts (name)
    , label         TEXT          NOT NULL
    , val           TEXT          
);

COMMIT;
