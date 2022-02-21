-- Revert avito-schema-test:add_realty_tname from pg

BEGIN;

DELETE FROM tfields WHERE tname = 'realty' AND name IN ('_postOperationType', '_postRooms');

DELETE FROM tnames WHERE tname = 'realty';

COMMIT;
