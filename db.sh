#!/bin/sh


alias mysql="mysql -h localhost -P 3306 -u marketplace -ppassword"



GRANT ALL PRIVILEGES ON *.* TO 'marketplace'@'%';

GRANT CREATE, ALTER, DROP, INDEX, CREATE TEMPORARY TABLES, LOCK TABLES, REFERENCES ON *.* TO 'marketplace'@'%';


ALTER USER 'marketplace'@'%' IDENTIFIED BY 'password';





FLUSH PRIVILEGES;

CREATE DATABASE service_seller;