-- Deploy avito-schema-test:insert_first_record to pg
-- requires: add_test_table

BEGIN;

INSERT INTO test_table (name, col1, col2, col3) VALUES ('first', 'Колонка1', 'Колонка2', 'Колонка3');

COMMIT;
