-- Verify avito-schema:add_test_table on pg

BEGIN;

SELECT id FROM test_table WHERE FALSE;

ROLLBACK;
