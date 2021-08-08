-- Revert avito-schema-test:insert_first_record from pg

BEGIN;

DELETE FROM test_table WHERE id = 0;

COMMIT;
