-- Verify avito-schema-test:add_posts on pg

BEGIN;

SELECT id FROM posts WHERE FALSE;

ROLLBACK;
