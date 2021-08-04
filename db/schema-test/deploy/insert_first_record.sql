-- Deploy avito-schema-test:insert_first_record to pg
-- requires: add_test_table

BEGIN;

INSERT INTO test_table (name) VALUES ('first');

COMMIT;
