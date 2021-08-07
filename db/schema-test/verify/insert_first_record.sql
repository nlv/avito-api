-- Verify avito-schema-test:insert_first_record on pg

BEGIN;

SELECT id FROM test_table WHERE id = 1;

ROLLBACK;
