-- Revert avito-schema:add_test_table from pg

BEGIN;

DROP TABLE test_table;

COMMIT;
