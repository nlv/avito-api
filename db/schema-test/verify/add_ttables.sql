-- Verify avito-schema-test:add_ttables on pg

BEGIN;

SELECT id FROM ttables WHERE FALSE;

ROLLBACK;
