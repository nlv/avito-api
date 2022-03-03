-- Revert avito-schema-test:not_null_posts_ttable_id from pg

BEGIN;

ALTER TABLE posts ALTER COLUMN ttable_id  DROP   NOT NULL;

COMMIT;
