-- Deploy avito-schema-test:insert_operations_types_opts to pg

BEGIN;

INSERT INTO opts (name) VALUES ('operations_types');

INSERT INTO opts_vals (optname, label, val)
VALUES
  ('operations_types', 'Продам', 'Продам')
, ('operations_types', 'Сдам', 'Сдам')
;

COMMIT;
