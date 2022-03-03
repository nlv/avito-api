-- Deploy avito-schema-test:not_null_posts_ttable_id to pg

BEGIN;

ALTER TABLE posts ALTER COLUMN ttable_id  SET   NOT NULL;

COMMIT;
