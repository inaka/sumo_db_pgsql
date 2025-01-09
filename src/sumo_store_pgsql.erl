%%% @hidden
%%% @doc PostgreSql store implementation.
%%%
%%% Copyright 2012 Inaka &lt;hello@inaka.net&gt;
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%% @end
%%% @copyright Inaka <hello@inaka.net>
%%%
-module(sumo_store_pgsql).
-author("Juan Facorro <juan@inaka.net>").
-license("Apache License 2.0").

-include_lib("epgsql/include/epgsql.hrl").

-behavior(sumo_store).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Exports.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% Public API.
-export([init/1]).
-export([create_schema/2]).
-export([persist/2]).
-export([delete_by/3, delete_all/2]).
-export([find_all/2, find_all/5, find_by/3, find_by/5, find_by/6]).
-export([fetch/3, count/2, count_by/3]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Types.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-type state() :: #{conn => term()}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% External API.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec init(term()) -> {ok, state()}.
init(Options) ->
  % The storage backend key in the options specifies the name of the process
  % which creates and initializes the storage backend.
  Backend = proplists:get_value(storage_backend, Options),
  Conn    = sumo_backend_pgsql:get_connection(Backend),
  {ok, #{conn => Conn}}.

-spec persist(Doc, State) -> Response when
  Doc      :: sumo_internal:doc(),
  State    :: state(),
  Response :: sumo_store:result(sumo_internal:doc(), state()).
persist(Doc,  #{conn := Conn} = State) ->
  DocName = sumo_internal:doc_name(Doc),
  IdField = sumo_internal:id_field_name(DocName),
  Id = sumo_internal:get_field(IdField, Doc),

  TableName = escape(DocName),

  Fields = sumo_internal:doc_fields(Doc),
  NPFields = maps:remove(IdField, Fields), % Non-primary fields.
  NPFieldNames = maps:keys(NPFields),
  NPColumnNames = lists:map(fun escape/1, NPFieldNames),

  Schema = sumo_internal:get_schema(DocName),
  SchemaFields = sumo_internal:schema_fields(Schema),
  ColumnTypes = [
    {sumo_internal:field_name(F), sumo_internal:field_type(F), sumo_internal:field_attrs(F)}
    || F <- SchemaFields
  ],

  NPColumnValues = lists:map(fun (N) ->
    {N, T, A} = lists:keyfind(N, 1, ColumnTypes),
    sleep_fun(T, N, maps:get(N, Fields), A)
  end, NPFieldNames),

  {Sql, Values} = case Id of
    undefined ->
      NPColumnsNamesCSV = string:join(NPColumnNames, ", "),

      SlotsFun = fun(N) -> [" $", integer_to_list(N), " "]  end,
      InsertSlots = lists:map(SlotsFun, lists:seq(1, length(NPFieldNames))),
      InsertSlotsCSV = string:join(InsertSlots, ", "),

      InsertSql = [
        "INSERT INTO ",
        TableName,
        " ( ", NPColumnsNamesCSV, " ) ",
        " VALUES ",
        " ( ", InsertSlotsCSV, " ) ",
        "RETURNING ", escape(IdField)
      ],

      InsertValues = NPColumnValues,

      {InsertSql, InsertValues};
    Id ->
      UpdateFun = fun(FieldName, {N, Slots}) ->
        Slot = [FieldName, " = $", integer_to_list(N), " "],
        {N + 1, [Slot | Slots]}
      end,

      {_, UpdateSlots} = lists:foldl(UpdateFun, {1, []}, NPColumnNames),
      UpdateSlotsCSV = string:join(UpdateSlots, ", "),

      NPCount = length(NPFieldNames),
      IdSlot = ["$", integer_to_list(NPCount + 1)],

      UpdateSql = [
        "UPDATE ", TableName,
        " SET ",
        UpdateSlotsCSV,
        " WHERE ",
        escape(IdField), " = ", IdSlot
      ],

      UpdateValues = NPColumnValues ++ [Id],

      {UpdateSql, UpdateValues}
  end,

  ToNullFun = fun
    (undefined) -> null;
    (Value) -> Value
  end,

  ProcessedValues = lists:map(ToNullFun, Values),
  case epgsql:equery(Conn, stringify(Sql), ProcessedValues) of
    {ok, _Count, _Columns, Rows} ->
      {LastId} = hd(Rows),
      NewDoc = sumo_internal:set_field(IdField, LastId, Doc),
      {ok, NewDoc, State};
    {ok, _Count} ->
      {ok, Doc, State};
    {error, Error} ->
      {error, Error, State}
  end.

-spec delete_by(DocName, Conditions, State) -> Response when
  DocName    :: sumo:schema_name(),
  Conditions :: sumo:conditions(),
  State      :: state(),
  Response   :: sumo_store:result(sumo_store:affected_rows(), state()).
delete_by(DocName, Conditions, #{conn := Conn} = State) ->
  {Values, CleanConditions} = sumo_sql_builder:values_conditions(Conditions),
  Clauses = sumo_sql_builder:where_clause(
    CleanConditions,
    fun escape/1,
    fun sumo_sql_builder:slot_numbered/1
  ),

  Sql = [
    "DELETE FROM ",
    escape(atom_to_list(DocName)),
    " WHERE ",
    lists:flatten(Clauses)
  ],

  case epgsql:equery(Conn, stringify(Sql), Values) of
    {ok, Count} ->
      {ok, Count, State};
    {error, Error} ->
      {error, Error, State}
  end.

-spec delete_all(DocName, State) -> Response when
  DocName  :: sumo:schema_name(),
  State    :: state(),
  Response :: sumo_store:result(sumo_store:affected_rows(), state()).
delete_all(DocName,  #{conn := Conn} = State) ->
  Sql = ["DELETE FROM ", escape(DocName)],

  case epgsql:equery(Conn, stringify(Sql), []) of
    {ok, Count} ->
      {ok, Count, State};
    {error, Error} ->
      {error, Error, State}
  end.

-spec find_all(DocName, State) -> Response when
  DocName  :: sumo:schema_name(),
  State    :: state(),
  Response :: sumo_store:result([sumo_internal:doc()], state()).
find_all(DocName, State) ->
  find_all(DocName, [], 0, 0, State).

-spec find_all(DocName, SortFields, Limit, Offset, State) -> Response when
  DocName    :: sumo:schema_name(),
  SortFields :: term(),
  Limit      :: non_neg_integer(),
  Offset     :: non_neg_integer(),
  State      :: state(),
  Response   :: sumo_store:result([sumo_internal:doc()], state()).
find_all(DocName, SortFields, Limit, Offset, State) ->
  find_by(DocName, [], SortFields, Limit, Offset, State).

-spec find_by(DocName, Conditions, State) -> Response when
  DocName    :: sumo:schema_name(),
  Conditions :: sumo:conditions(),
  State      :: state(),
  Response   :: sumo_store:result([sumo_internal:doc()], state()).
find_by(DocName, Conditions, State) ->
  find_by(DocName, Conditions, [], 0, 0, State).

-spec find_by(DocName, Conditions, Limit, Offset, State) -> Response when
  DocName    :: sumo:schema_name(),
  Conditions :: sumo:conditions(),
  Limit      :: non_neg_integer(),
  Offset     :: non_neg_integer(),
  State      :: state(),
  Response   :: sumo_store:result([sumo_internal:doc()], state()).
find_by(DocName, Conditions, Limit, Offset, State) ->
  find_by(DocName, Conditions, [], Limit, Offset, State).

%% XXX We should have a DSL here, to allow querying in a known language
%% to be translated by each driver into its own.
-spec find_by(DocName, Conditions, Sort, Limit, Offset, State) -> Response when
  DocName    :: sumo:schema_name(),
  Conditions :: sumo:conditions(),
  Sort       :: term(),
  Limit      :: non_neg_integer(),
  Offset     :: non_neg_integer(),
  State      :: state(),
  Response   :: sumo_store:result([sumo_internal:doc()], state()).
find_by(DocName, Conditions, SortFields, Limit, Offset, #{conn := Conn} = State) ->
  {Values, CleanConditions} = sumo_sql_builder:values_conditions(Conditions),
  Clauses = sumo_sql_builder:where_clause(
    CleanConditions,
    fun escape/1,
    fun sumo_sql_builder:slot_numbered/1
  ),

  OrderByClause = case SortFields of
    [] -> [];
    _  -> sumo_sql_builder:order_by_clause(SortFields, fun escape/1)
  end,

  WhereClause = case Conditions of
    [] -> "";
    _  -> [" WHERE ", lists:flatten(Clauses)]
  end,

  %% Select * is not good..
  Sql1 = [
    "SELECT * FROM ",
    escape(atom_to_list(DocName)),
    WhereClause,
    OrderByClause
  ],

  Sql2 = case Limit of
    0 ->
      Sql1;
    _ ->
      Count = length(Values),
      [Sql1 | [" LIMIT $", integer_to_list(Count + 1), " OFFSET $", integer_to_list(Count + 2)]]
  end,

  AllValues = case Limit of
    0     -> Values;
    Limit -> Values ++ [Limit, Offset]
  end,

  case epgsql:equery(Conn, stringify(Sql2), AllValues) of
    {ok, Columns, Rows} ->
      ColFun = fun(Col) -> binary_to_atom(Col#column.name, utf8) end,
      ColumnNames = lists:map(ColFun, Columns),

      FoldFun = fun({Name, Value}, Doc) ->
        sumo_internal:set_field(Name, Value, Doc)
      end,
      RowFun = fun(Row) ->
        Fields = tuple_to_list(Row),
        Pairs = lists:zip(ColumnNames, Fields),
        NewDoc = sumo_internal:new_doc(DocName),
        wakeup(lists:foldl(FoldFun, NewDoc, Pairs))
      end,
      Docs = lists:map(RowFun, Rows),
      {ok, Docs, State};
    {error, Error} ->
      {error, Error, State}
  end.

-spec fetch(DocName, Id, State) -> Response when
  DocName  :: sumo:schema_name(),
  Id       :: sumo:field_value(),
  State    :: state(),
  Response :: sumo_store:result(sumo_internal:doc(), state()).
fetch(DocName, Id, State) ->
  IdFieldName = sumo_internal:id_field_name(DocName),
  case find_by(DocName, [{IdFieldName, Id}], [], 1, 0, State) of
    {ok, [Doc], _} -> {ok, Doc, State};
    {ok, [], _}    -> {error, notfound, State};
    Error          -> Error
  end.

-spec count(DocName, State) -> Response when
  DocName  :: sumo:schema_name(),
  State    :: state(),
  Response :: sumo_store:result(non_neg_integer(), state()).
count(DocName, #{conn := Conn} = State) ->
  case epgsql:squery(Conn, ["SELECT Count(*) FROM ", escape(atom_to_list(DocName))]) of
    {ok, _, [{Count}]} ->
      {ok, binary_to_integer(Count), State};
    {error, Error} ->
      {error, Error, State}
  end.

-spec count_by(DocName, Conditions, State) -> Response when
  DocName    :: sumo:schema_name(),
  Conditions :: sumo:conditions(),
  State      :: state(),
  Response   :: sumo_store:result(non_neg_integer(), state()).
count_by(DocName, [], State) ->
  count(DocName, State);
count_by(DocName, Conditions, #{conn := Conn} = State) ->
  {Values, CleanConditions} = sumo_sql_builder:values_conditions(Conditions),
  Clauses = sumo_sql_builder:where_clause(
    CleanConditions,
    fun escape/1,
    fun sumo_sql_builder:slot_numbered/1
  ),

  Sql = [
    "SELECT COUNT(1) FROM ",
    escape(atom_to_list(DocName)),
    " WHERE ",
    lists:flatten(Clauses)
  ],

  case epgsql:equery(Conn, stringify(Sql), Values) of
    {ok, _, [{Count}]} ->
      {ok, Count, State};
    {error, Error} ->
      {error, Error, State}
  end.

%% XXX: Refactor:
%% Requires {length, X} to be the first field attribute in order to form the
%% correct query. :P
%% If no indexes are defined, will put an extra comma :P
%% Maybe it would be better to just use ALTER statements instead of trying to
%% create the schema on the 1st pass. Also, ALTER statements might be better
%% for when we have migrations.
-spec create_schema(sumo:schema(), state()) -> sumo_store:result(state()).
create_schema(Schema, #{conn := Conn} = State) ->
  Name = sumo_internal:schema_name(Schema),
  Fields = sumo_internal:schema_fields(Schema),
  FieldsDql = lists:map(fun create_column/1, Fields),

  Indexes = lists:filter(
    fun(T) -> length(T) > 0 end,
    lists:map(fun create_index/1, Fields)
  ),
  Dql = [
    "CREATE TABLE IF NOT EXISTS ", escape(atom_to_list(Name)), " (",
    string:join(FieldsDql, ", "), ", ", string:join(Indexes, ", "),
    ") "
  ],
  BinDql = iolist_to_binary(Dql),
  StrDql = binary_to_list(BinDql),

  case epgsql:squery(Conn, StrDql) of
    {error, Error} -> {error, Error, State};
    {ok, [], []} -> {ok, State}
  end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Private API.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @private
escape(Name) when is_atom(Name) ->
  ["\"", atom_to_list(Name), "\""];
escape(String) ->
  ["\"", String, "\""].

%%% Schema related

%% @private
create_column(Field) ->
  create_column(
    sumo_internal:field_name(Field),
    sumo_internal:field_type(Field),
    sumo_internal:field_attrs(Field)).

  %% @private
create_column(Name, integer, Attrs) ->
  case lists:member(auto_increment, Attrs) of
    true ->
      AttrsNoAutoInc = lists:delete(auto_increment, Attrs),
      [escape(atom_to_list(Name)),
       " SERIAL ",
       create_column_options(AttrsNoAutoInc)];
    false ->
      [escape(atom_to_list(Name)), " INTEGER ", create_column_options(Attrs)]
  end;
create_column(Name, bigint, Attrs) ->
    [atom_to_list(Name), " BIGINT", create_column_options(Attrs)];
create_column(Name, float, Attrs) ->
  [escape(atom_to_list(Name)), " FLOAT ", create_column_options(Attrs)];
create_column(Name, binary, Attrs) ->
  [escape(atom_to_list(Name)), " BYTEA ", create_column_options(Attrs)];
create_column(Name, string, Attrs) ->
  [escape(atom_to_list(Name)), " VARCHAR ", create_column_options(Attrs)];
create_column(Name, date, Attrs) ->
  [escape(atom_to_list(Name)), " DATE ", create_column_options(Attrs)];
create_column(Name, datetime, Attrs) ->
  [escape(atom_to_list(Name)), " TIMESTAMP ", create_column_options(Attrs)];
create_column(Name, boolean, Attrs) ->
  [escape(atom_to_list(Name)), " BOOLEAN ", create_column_options(Attrs)];
create_column(Name, custom, Attrs) ->
  case lists:keyfind(type, 1, Attrs) of
    {type, text} ->
      [escape(atom_to_list(Name)), " TEXT ", create_column_options(Attrs)];
    _ ->
      create_column(Name, binary, Attrs)
  end.

%% @private
create_column_options(Attrs) ->
  Options = lists:map(fun create_column_option/1, Attrs),
  lists:filter(fun is_list/1, Options).

  %% @private
create_column_option(auto_increment) ->
  throw({badarg, "Only integer can be auto_increment"});
create_column_option(not_null) ->
  [" NOT NULL "];
create_column_option({length, X}) ->
  ["(", integer_to_list(X), ") "];
create_column_option(_Option) ->
  none.

%% @private
create_index(Field) ->
  Name = sumo_internal:field_name(Field),
  Attrs = sumo_internal:field_attrs(Field),
  Attrs1 = [create_index(Name, Attr) || Attr <- Attrs],
  lists:filter(fun is_list/1, Attrs1).

  %% @private
create_index(Name, id) ->
  ["PRIMARY KEY(", escape(atom_to_list(Name)), ")"];
create_index(Name, unique) ->
  List = atom_to_list(Name),
  ["UNIQUE ", " (", escape(List), ")"];
create_index(Name, index) ->
  List = atom_to_list(Name),
  ["KEY ", escape(List), " (", escape(List), ")"];
create_index(_, _) ->
  none.

%% @todo remove this once pgsql specs are fixed to support iodata and make
%%       dialyzer happy
%% @private
stringify(Sql) -> binary_to_list(iolist_to_binary(Sql)).

%% @private
wakeup(Doc) ->
  sumo_utils:doc_transform(fun wakeup_fun/4, Doc).

%% @private
%% Matches `text' type fields that were saved with `undefined' value and
%% avoids being processed by the next clause that will return it as a
%% binary (`<<"undefined">>') instead of atom as expected.
wakeup_fun(_, _, null, _) ->
  undefined;
wakeup_fun(string, _, FieldValue, _) ->
  sumo_utils:to_bin(FieldValue);
wakeup_fun(custom, FieldName, FieldValue, Attrs) ->
  case lists:keyfind(type, 1, Attrs) of
    {type, text} ->
      wakeup_fun(string, FieldName, FieldValue, Attrs);
    _ ->
      binary_to_term(FieldValue)
  end;
wakeup_fun(_, _, FieldValue, _) ->
  FieldValue.

%% @private
sleep_fun(_, _, null, _) ->
  undefined;
sleep_fun(custom, _, FieldValue, Attrs) ->
  case lists:keyfind(type, 1, Attrs) of
    {type, text} -> FieldValue;
    _ -> term_to_binary(FieldValue)
  end;
sleep_fun(_, _, FieldValue, _) ->
  FieldValue.
