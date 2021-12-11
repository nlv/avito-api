-- Verify avito-schema-test:add_opts_table on pg

BEGIN;

SELECT id FROM opts WHERE FALSE;

ROLLBACK;
