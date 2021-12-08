-- Revert avito-schema-test:add_posts from pg

BEGIN;

DROP TABLE posts;

COMMIT;
