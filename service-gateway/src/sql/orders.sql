CREATE DATABASE service_gateway;

use service_gateway;

create table if not exists orders(
  id varchar(20) not null,
  mode varchar(20) not null,
  status varchar(20) default "created",
  deployed boolean default false,
  seller_id varchar(20) not null,
  seller_pubkeyhash varchar(100) default null,
  buyer_id varchar(20) default null,
  buyer_pubkeyhash varchar(100) default null,
  contract_address varchar(100) default null,
  contract_state int unsigned not null,
  contract_threadtoken varchar(100) default null,
  contract_unit varchar(100) default null,
  contract_units int unsigned not null,
  contract_price int unsigned not null,
  contract_collateral int unsigned not null,
  contract_0_tx varchar(100) default null,
  contract_1_tx varchar(100) default null,
  contract_2_tx varchar(100) default null,
  contract_3_tx varchar(100) default null,  
  product_id varchar(20) not null,
  product_discount int unsigned not null,
  created_at timestamp default current_timestamp,
  schema_t timestamp default current_timestamp,
  schema_v int unsigned not null,
  primary key(id),
  unique (contract_id)
) ENGINE = InnoDB;