-- Deploy avito-schema:add_test_table to pg

BEGIN;

CREATE TABLE test_table (
     id           SERIAL      PRIMARY KEY
    ,name         TEXT        NOT NULL UNIQUE
    ,col1         TEXT        
    ,col2         TEXT        
    ,col3         TEXT        
);

COMMIT;
