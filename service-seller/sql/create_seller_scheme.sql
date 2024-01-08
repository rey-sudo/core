create table if not exists seller(
  seller_id bigint not null auto_increment,
  nickname varchar(50) not null,
  email varbinary(128) not null,
  password_hash varbinary(128) not null,
  verified boolean default false,
  avatar_url varchar(255) not null,
  public_ip varchar(50) not null,
  created_at timestamp default current_timestamp,
  last_login timestamp default current_timestamp
  schema_t timestamp default current_timestamp,
  schema_v bigint not null auto_increment,
  primary key(seller_id, schema_v)
) ENGINE=InnoDB;

