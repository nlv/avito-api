-- Deploy avito-schema-test:insert_dump_ttable to pg

BEGIN;

INSERT INTO ttables (tname, name)
VALUES ('for_house', 'for_house dump table for linking posts to ttables')
,('realty', 'realty dump table for linking posts to ttables')
;

UPDATE posts SET ttable_id = (SELECT id FROM ttables WHERE name = 'for_house dump table for linking posts to ttables' AND tname = 'for_house')
WHERE tname = 'for_house';

UPDATE posts SET ttable_id = (SELECT id FROM ttables WHERE name = 'realty dump table for linking posts to ttables' AND tname = 'for_house')
WHERE tname = 'realty';

COMMIT;
