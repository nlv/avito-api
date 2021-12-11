-- Verify avito-schema-test:add_tnames_table on pg

BEGIN;

SELECT id FROM tnames WHERE FALSE;

ROLLBACK;
