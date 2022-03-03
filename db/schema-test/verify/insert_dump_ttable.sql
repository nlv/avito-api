-- Verify avito-schema-test:insert_dump_ttable on pg

BEGIN;

SELECT 1/COUNT(*) FROM ttables WHERE name IN (
'for_house dump table for linking posts to ttables',
'realty', 'realty dump table for linking posts to ttables'
)
;


ROLLBACK;
