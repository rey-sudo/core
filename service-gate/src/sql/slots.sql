CREATE DATABASE service_gate;

create table if not exists slots(
  id varchar(20) not null,
  mode varchar(20) not null,
  status varchar(20) default "created",
  deployed boolean default false,
  wallet_id varchar(100) not null,
  contract_id varchar(100) not null,
  contract_state varchar(20) default null,
  contract_0_status mediumtext default null,
  contract_1_status mediumtext default null,
  contract_2_status mediumtext default null,
  contract_3_status mediumtext default null,
  contract_4_status mediumtext default null,
  seller_id varchar(20) not null,
  seller_pkh varchar(100) default null,
  buyer_id varchar(20) default null,
  buyer_pkh varchar(100) default null,
  product_id varchar(20) not null,
  product_price int unsigned not null,
  product_collateral int unsigned not null,
  product_discount int unsigned not null,
  product_units int unsigned not null,
  created_at timestamp default current_timestamp,
  schema_t timestamp default current_timestamp,
  schema_v int unsigned not null,
  primary key(id),
  unique (contract_id)
) ENGINE = InnoDB;







