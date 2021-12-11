-- Revert avito-schema-test:add_for_house_tname from pg

BEGIN;

DELETE FROM tnames_fields WHERE tname = 'for_house' AND name IN ('_postGoodsType');

DELETE FROM tnames WHERE tname = 'for_house';

COMMIT;
