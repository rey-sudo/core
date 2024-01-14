#!/bin/sh

alias vtctldclient="vtctldclient --server=localhost:15999"
alias mysql="mysql -h 127.0.0.1 -P 15306 -u marketplace -ppassword"



vtctldclient ApplySchema --sql-file="service-seller/src/sql/create_seller_scheme.sql" service_seller

vtctldclient ApplyVSchema --vschema-file="service-seller/src/sql/create_seller_scheme.json" service_seller
