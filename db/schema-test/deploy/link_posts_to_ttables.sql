-- Deploy avito-schema-test:link_posts_to_ttables to pg

BEGIN;

ALTER TABLE posts ADD COLUMN ttable_id     INT           REFERENCES ttables(id);

COMMIT;
