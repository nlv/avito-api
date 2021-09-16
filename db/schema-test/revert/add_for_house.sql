-- Revert avito-schema-test:add_for_house from pg

BEGIN;

DROP TABLE for_house;

COMMIT;
