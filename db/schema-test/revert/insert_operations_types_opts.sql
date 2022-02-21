-- Revert avito-schema-test:insert_operations_types_opts from pg

BEGIN;

DELETE FROM opts_vals WHERE optname = 'operations_types' AND label IN ('Продам', 'Сдам');

DELETE FROM opts WHERE name = 'operations_types';

COMMIT;
