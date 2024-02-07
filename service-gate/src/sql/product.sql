create table if not exists product(
  product_id varchar(20) not null,
  seller_id varchar(20) not null,
  title varchar(125) not null,
  category varchar(100) not null,
  price int unsigned default 0,
  collateral int unsigned default 0,
  stock int unsigned default 0,
  slots int unsigned default 0,
  note varchar(500) not null,
  keywords varchar(100) not null,
  theme varchar(500) not null,
  terms varchar(1000) not null,
  country varchar(10) not null,
  moderated boolean default false,
  image_base varchar(255) not null,
  image_path varchar(255) not null,
  created_at timestamp default current_timestamp,
  schema_t timestamp default current_timestamp,
  schema_v int unsigned not null,
  primary key(product_id)
) ENGINE=InnoDB;

