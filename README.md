# sumo_db_pgsql

# About

This is the [PostgreSQL](http://www.postgresql.org/download/) adapter for [sumo_db](https://github.com/inaka/sumo_db).

## PosgreSQL

### Install PostgreSQL

To install **PostgreSQL** please follow the instructions in this link:
[Installing PostgreSQL](https://wiki.postgresql.org/wiki/Detailed_installation_guides).

## Getting Started

To start using `sumo_db` with this PostgreSQL adapter `sumo_db_pgsql` is pretty easy, you just have to follow these
steps:

1. Add `sumo_db` and `sumo_db_pgsql` as dependencies in your project.

   Using **Rebar3**:
  
   ```erlang
   {deps, [
     {sumo_db_pgsql, "0.1.1"}
   ]}.
   ```

1. You need at least one doc/entity, let's use [sumo_test_people_pgsql](./test/sumo_test_people_pgsql.erl) as example.
   > NOTE: if you use this entity, you'll need to include `mixer` to the dependencies list

1. Provide the configuration file, e.g.: [test.config](./test/test.config).

1. Now you can run your app and start using `sumo` from there.

### Running `sumo` from Erlang console

Start the Erlang console, adding the path to your beams and config file

```shell
$ REBAR_PROFILE=test rebar3 shell --config=test/test.config
```

Within the console:

```erlang
> application:ensure_all_started(sumo_db_pgsql).
12:02:00.250 [info] Application lager started on node nonode@nohost
12:02:00.250 [info] Application crypto started on node nonode@nohost
12:02:00.251 [info] Application asn1 started on node nonode@nohost
12:02:00.251 [info] Application public_key started on node nonode@nohost
12:02:00.251 [info] Application ssl started on node nonode@nohost
12:02:00.252 [info] Application epgsql started on node nonode@nohost
12:02:00.256 [info] Application sasl started on node nonode@nohost
12:02:00.256 [info] Creating wpool ETS table
12:02:00.256 [info] Application worker_pool started on node nonode@nohost
12:02:00.256 [info] Application quickrand started on node nonode@nohost
12:02:00.256 [info] Application uuid started on node nonode@nohost
12:02:00.337 [info] Application sumo_db started on node nonode@nohost
12:02:00.341 [info] Application sumo_db_pgsql started on node nonode@nohost
{ok,[syntax_tools,compiler,goldrush,lager,crypto,asn1,
     public_key,ssl,epgsql,sasl,worker_pool,quickrand,uuid,
     sumo_db,sumo_db_pgsql]}

% from here you can start using sumo

> sumo:find_all(people).
[]
```

## Running Tests

A PostgreSQL instance is needed to run the tests. If `Docker` (and `Docker Compose`) is an option, a PostgreSQL server
can be run executing the following commands (assuming the current/working directory is the project's root):

```shell
$ docker-compose up -d
```

After this is completed, a similar result should be displayed:

```shell
$ docker-compose ps
      Name                    Command              State           Ports
---------------------------------------------------------------------------------
sumodbpgsql_db_1   docker-entrypoint.sh postgres   Up      0.0.0.0:5432->5432/tcp
```

If `Docker` (and `Docker Compose`) is not an alternative, install PostgreSQL (refer to **Install PostgreSQL**) and make
sure to:

- Create a `postgres` user (with `postgres` as password).
- Create a _test_ `sumo_test` database.

> **NOTE:** Alternatively, use PostgreSQL defaults and make sure to modify/update `test/test.config` accordingly.

Then run:

```shell
$ rebar3 ct
```

## Contact Us

If you find any **bugs** or have a **problem** while using this library, please
[open an issue](https://github.com/inaka/sumo_db_pgsql/issues/new) in this repo (or a pull request :)).
