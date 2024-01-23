create table if not exists order(
  order_id varchar(20) not null,
  buyer_id varchar(20) not null,
  product_id varchar(20) not null,
  instance_id varchar(50) not null,
  order_status varchar(20) not null,
  wallet_id varchar(56) not null,
  seller_pkh varchar(56) not null,
  buyer_pkh varchar(56) default "unset",
  product_price int unsigned default 0,
  collateral int unsigned default 0,
  schema_t timestamp default current_timestamp,
  schema_v int unsigned not null
  primary key(order_id)
) ENGINE=InnoDB;



