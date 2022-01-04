-- Verify avito-schema-test:add_tfields_table on pg

BEGIN;

SELECT id FROM tfields WHERE FALSE;

ROLLBACK;
