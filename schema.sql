CREATE DATABASE formula_one;

\c formula_one

CREATE TABLE season (
  id serial PRIMARY KEY
);

CREATE TABLE race (
  id serial PRIMARY KEY,
  season_id int8 NOT NULL REFERENCES season(id),
  name text NOT NULL,
  location text NOT NULL,
  start_time timestamptz
);

CREATE TABLE race_tier (
  id serial PRIMARY KEY,
  race_id int8 NOT NULL REFERENCES race(id),
  point_value int8 NOT NULL,
  UNIQUE (race_id, point_value)
);

CREATE TABLE driver (
  id serial PRIMARY KEY,
  first_name text NOT NULL,
  last_name text NOT NULL,
  team text NOT NULL
);

CREATE TABLE race_driver (
  race_id int8 NOT NULL REFERENCES race(id),
  driver_id int8 NOT NULL REFERENCES driver(id),
  tier_id int8 NOT NULL REFERENCES race_tier(id),
  UNIQUE (race_id, driver_id, tier_id)
);
