-- Revert avito-schema-test:add_ttables from pg

BEGIN;

DROP TABLE ttables;

COMMIT;
