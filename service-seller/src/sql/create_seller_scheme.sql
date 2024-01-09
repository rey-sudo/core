create table if not exists seller(
  seller_id varchar(20) not null,
  nickname varchar(50) not null,
  email varbinary(128) not null,
  password_hash varbinary(128) not null,
  verified boolean default false,
  country varchar(50) not null,
  completed_sales int default 0,
  uncompleted_sales int default 0,
  terms varchar(1000) not null,
  avatar_url varchar(255) not null,
  public_ip varchar(50) not null,
  created_at timestamp default current_timestamp,
  last_login timestamp default current_timestamp,
  schema_t timestamp default current_timestamp,
  schema_v int not null auto_increment,
  primary key(seller_id),
  unique key(schema_v)
) ENGINE=InnoDB;

