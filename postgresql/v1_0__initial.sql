create extension citext;
create extension pgcrypto;

create table users (
  task_id bigserial primary key not null,
  task_name text not null,
  task_owner text not null,
  task_completed boolean default false,
);
