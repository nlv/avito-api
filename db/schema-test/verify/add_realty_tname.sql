-- Verify avito-schema-test:add_realty_tname on pg

BEGIN;

SELECT 1/COUNT(*) FROM tfields WHERE tname = 'realty' AND name IN ('_postOperationType', '_postRooms');


ROLLBACK;
