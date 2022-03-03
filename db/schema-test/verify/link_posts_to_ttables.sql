-- Verify avito-schema-test:link_posts_to_ttables on pg

BEGIN;

SELECT ttable_id FROM posts WHERE FALSE;

ROLLBACK;
