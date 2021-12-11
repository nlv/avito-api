-- Revert avito-schema-test:add_opts_table from pg

BEGIN;

DROP TABLE opts;

COMMIT;
