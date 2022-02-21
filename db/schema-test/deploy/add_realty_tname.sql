-- Deploy avito-schema-test:add_realty_tname to pg

BEGIN;

INSERT INTO tnames (tname, label)
VALUES ('realty', 'Недвижимость');

INSERT INTO tfields (tname, name, label, ftype, optname)
VALUES 
('realty', '_postOperationType', 'Тип объявления', 'enum', 'operations_types'),
('realty', '_postRooms', 'Кол-во комнат', 'number', NULL)
;

COMMIT;
