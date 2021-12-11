-- Revert avito-schema-test:add_tnames_fields_table from pg

BEGIN;

DROP TABLE tnames_fields;

COMMIT;
