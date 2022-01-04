-- Deploy avito-schema-test:add_for_house_tname to pg
-- requires: add_tnames_table

BEGIN;

INSERT INTO tnames (tname, label)
VALUES ('for_house', 'Для дома и дачи');

INSERT INTO tfields (tname, name, label, ftype, optname)
VALUES ('for_house', '_postGoodsType', 'Вид товара', 'enum', 'goods_types');

COMMIT;
