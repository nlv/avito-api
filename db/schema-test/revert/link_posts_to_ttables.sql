-- Revert avito-schema-test:link_posts_to_ttables from pg

BEGIN;

ALTER TABLE posts DROP COLUMN ttable_id;

COMMIT;
