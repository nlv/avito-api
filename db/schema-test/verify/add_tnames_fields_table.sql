-- Verify avito-schema-test:add_tnames_fields_table on pg

BEGIN;

SELECT id FROM tnames_fields WHERE FALSE;

ROLLBACK;
