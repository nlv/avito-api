-- Verify avito-schema-test:add_for_house on pg

BEGIN;

SELECT id FROM for_house WHERE FALSE;

ROLLBACK;
