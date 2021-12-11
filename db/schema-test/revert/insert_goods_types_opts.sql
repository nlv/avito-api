-- Revert avito-schema-test:insert_goods_types_opts from pg

BEGIN;

DELETE FROM opts_vals WHERE optname = 'goods_types' AND label IN ('Кондиционеры', 'Изоляция');

DELETE FROM opts WHERE name = 'goods_types';

COMMIT;
