-- Deploy avito-schema:add_test_table to pg

BEGIN;

CREATE TABLE test_table (
     id           INT    PRIMARY KEY CHECK (id > 0)
    ,col1         TEXT        
    ,col2         TEXT        
    ,col3         TEXT        
);

COMMIT;
