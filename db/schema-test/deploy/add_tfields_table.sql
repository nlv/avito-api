-- Deploy avito-schema-test:add_tfields_table to pg
-- requires: add_tnames_table

BEGIN;

CREATE TABLE tfields (
      id            BIGSERIAL     PRIMARY KEY 
    , tname         TEXT          NOT NULL REFERENCES tnames (tname)
    , name          TEXT          NOT NULL  
    , label         TEXT
    , ftype         TEXT          NOT NULL CHECK (ftype IN ('text', 'enum', 'number'))
    , optname       TEXT          REFERENCES opts (name)
    CHECK ((ftype = 'enum' AND optname IS NOT NULL) OR (ftype != 'enum' AND optname IS NULL))
);

COMMIT;
