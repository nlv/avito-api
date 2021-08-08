-- Deploy avito-schema-test:insert_first_record to pg
-- requires: add_test_table

BEGIN;

INSERT INTO test_table (id, col1, col2, col3) VALUES (0, 'Колонка1', 'Колонка2', 'Колонка3');

COMMIT;
