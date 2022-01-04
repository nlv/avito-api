-- Verify avito-schema-test:add_for_house_tname on pg

BEGIN;

SELECT 1/COUNT(*) FROM tfields WHERE tname = 'for_house' AND name IN ('_postGoodsType');

ROLLBACK;
