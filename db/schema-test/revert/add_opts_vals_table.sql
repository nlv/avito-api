-- Revert avito-schema-test:add_opts_vals_table from pg

BEGIN;

DROP TABLE opts_vals;

COMMIT;
