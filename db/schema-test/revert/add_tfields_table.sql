-- Revert avito-schema-test:add_tfields_table from pg

BEGIN;

DROP TABLE tfields;

COMMIT;
