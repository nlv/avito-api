-- Verify avito-schema-test:insert_operations_types_opts on pg

BEGIN;

SELECT 1/COUNT(*) FROM opts_vals WHERE optname = 'operations_types' AND label IN ('Продам');

SELECT 1/COUNT(*) FROM opts_vals WHERE optname = 'operations_types' AND label IN ('Сдам');

ROLLBACK;
