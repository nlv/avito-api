-- Deploy avito-schema-test:insert_goods_types_opts to pg
-- requires: add_opts_vals_table

BEGIN;

INSERT INTO opts (name) VALUES ('goods_types');

INSERT INTO opts_vals (optname, label, val)
VALUES
  ('goods_types', 'Кондиционеры', 'Кондиционеры')
, ('goods_types', 'Изоляция', 'Изоляция')
;

COMMIT;
