CREATE DATABASE service_gate;


create table if not exists slot(
  slot_id varchar(20) not null,
  seller_id varchar(20) not null,
  buyer_id varchar(20) default "unset",
  product_id varchar(20) not null,
  instance_id varchar(100) not null,
  wallet_id varchar(100) not null,
  stage varchar(20) default "created",
  seller_pkh varchar(56) default "unset",
  buyer_pkh varchar(56) default "unset",
  product_price int unsigned default 0,
  collateral int unsigned default 0,
  created_at timestamp default current_timestamp,
  schema_t timestamp default current_timestamp,
  schema_v int unsigned not null,
  primary key(slot_id),
  unique (instance_id)
) ENGINE=InnoDB;



