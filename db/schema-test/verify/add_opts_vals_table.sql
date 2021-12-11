-- Verify avito-schema-test:add_opts_vals_table on pg

BEGIN;

SELECT id FROM opts_vals WHERE FALSE;

ROLLBACK;
