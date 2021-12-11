-- Revert avito-schema-test:add_tnames_table from pg

BEGIN;

DROP TABLE tnames;

COMMIT;
