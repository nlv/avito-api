-- Revert avito-schema-test:insert_dump_ttable from pg

BEGIN;

DELETE FROM  ttables WHERE name IN (
'for_house dump table for linking posts to ttables',
'realty', 'realty dump table for linking posts to ttables'
)
;

COMMIT;
