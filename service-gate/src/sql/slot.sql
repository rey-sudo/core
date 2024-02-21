CREATE DATABASE service_gate;


create table if not exists slot(
  slot_id varchar(20) not null,
  status varchar(20) default "created",
  wallet_id varchar(100) not null,
  instance_id varchar(100) not null,
  status_start varchar(1000) default "unset",
  status_locking varchar(1000) default "unset",
  status_delivered varchar(1000) default "unset",
  status_received varchar(1000) default "unset",
  seller_id varchar(20) not null,
  seller_pkh varchar(56) default "unset",
  buyer_id varchar(20) default "unset",
  buyer_pkh varchar(56) default "unset",
  product_id varchar(20) not null,
  product_price int unsigned default 0,
  product_collateral int unsigned default 0,
  created_at timestamp default current_timestamp,
  schema_t timestamp default current_timestamp,
  schema_v int unsigned not null,
  primary key(slot_id),
  unique (instance_id)
) ENGINE=InnoDB;



