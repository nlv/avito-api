-- Deploy avito-schema-test:add_tnames_fields_table to pg
-- requires: add_tnames_table

BEGIN;

CREATE TABLE tnames_fields (
      id            BIGSERIAL     PRIMARY KEY 
    , tname         TEXT          NOT NULL REFERENCES tnames (tname)
    , name          TEXT          NOT NULL  
    , label         TEXT
    , ftype         TEXT          NOT NULL CHECK (ftype IN ('text', 'enum', 'number'))
    , optname       TEXT          REFERENCES opts (name)
    CHECK ((ftype = 'enum' AND optname IS NOT NULL) OR (ftype != 'enum' AND optname IS NULL))
);

COMMIT;
