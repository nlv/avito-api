-- Verify avito-schema-test:insert_goods_types_opts on pg

BEGIN;

SELECT 1/COUNT(*) FROM opts_vals WHERE optname = 'goods_types' AND label IN ('Кондиционеры');

SELECT 1/COUNT(*) FROM opts_vals WHERE optname = 'goods_types' AND label IN ('Изоляция');


ROLLBACK;
