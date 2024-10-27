-module(trailify_open_api_converter).

-include_lib("yamerl/include/yamerl_nodes.hrl").
-include("trailify.hrl").

-export([to_trails/2]).

-type options() :: #{handler => {yaml, YamlRef :: atom()} | Handler :: module() | undefined,
                     server => any(),
                     generate_trails => boolean()}.

-type instance() :: #{openapi => binary(),
                      info => map(),
                      components => map(),
                      servers => list(),
                      tags => list(),
                      paths => map()}.

-export_type([ instance/0
             , options/0]).

to_trails(FilePath, Options) ->
  {HandlerRef, PropertySpec} =
    identify_handler(maps:get(handler, Options, undefined), ?PROPERTY_SPEC),
  try
    [YamlDoc] = yamerl_constr:file(FilePath, ?OPTIONS),
    Instance =
      case get_initial_tokens(YamlDoc) of
        {root, Root} -> convert_tokens(PropertySpec, Root, []);
        _ -> throw({error, root_not_found})
      end,
    NormalizedInstance = normalize_paths(Instance),
    case maps:get(generate_trails, Options, true) of
      true ->
        Trails =
          generate_trails(HandlerRef, maps:get(server, Options, undefined), NormalizedInstance),
        ok = save_to_global_spec(NormalizedInstance),
        {ok, Trails};
      false ->
        {ok, NormalizedInstance}
    end
  catch
    Error -> Error
  end.

normalize_paths(Instance) ->
  Paths =
    maps:fold(
      fun(Path, Meta, Accu) ->
        Accu#{normalize(Path) => Meta}
      end, #{}, maps:get(paths, Instance)),
  Instance#{paths => Paths}.

normalize(Url0) ->
  Normalized = lists:map(fun normalize_path_param/1, re:split(Url0, "/")),
  Url1 = lists:foldl(
    fun(<<>>, Accu) ->
      <<Accu/binary, "/">>;
       (E, Accu) ->
         <<Accu/binary, E/binary, "/">>
    end, <<>>, Normalized),
  binary:part(Url1, 0, byte_size(Url1) - 1).

normalize_path_param(<<>>) ->
  <<>>;
normalize_path_param(Param) ->
  case {binary:first(Param), binary:last(Param)} of
    {${, $}} ->
      <<":", (binary:part(Param, 1, byte_size(Param) - 2))/binary>>;
    _ ->
      Param
  end.

identify_handler({yaml, HandlerRef}, #{'$operation' := Operation} = PropertySpec) ->
  {{ref, HandlerRef},
   PropertySpec#{'$operation' => Operation#{atom_to_binary(HandlerRef) => atom}}};
identify_handler(HandlerMod, PropertySpec) ->
  {{mod, HandlerMod}, PropertySpec}.

get_initial_tokens(#yamerl_doc{root = Root}) ->
  {root, get_initial_tokens(Root)};
get_initial_tokens(#yamerl_map{pairs = Pairs}) ->
  {map, construct(map, Pairs)};
get_initial_tokens(#yamerl_seq{entries = Entries}) ->
  {seq, construct(seq, Entries)};
get_initial_tokens(#yamerl_str{text = Text}) ->
  {string, Text};
get_initial_tokens(#yamerl_null{}) ->
  {null, undefined};
get_initial_tokens(#yamerl_bool{value = Value}) ->
  {bool, Value};
get_initial_tokens(#yamerl_int{value = Value}) ->
  {int, Value};
get_initial_tokens(#yamerl_float{value = Value}) ->
  {float, Value};
get_initial_tokens(#yamerl_erlang_atom{name = Value}) ->
  {atom, Value};
get_initial_tokens(#yamerl_erlang_fun{function = Value}) ->
  {function, Value};
get_initial_tokens(#yamerl_timestamp{year = undefined, month = undefined, day = undefined,
                                     hour = H, minute = M, second = S}) ->
  {time, {H, M, S}};
get_initial_tokens(#yamerl_timestamp{hour = 0, minute = 0, second = 0,
                                     year = Y, month = M, day = D}) ->
  {date, {Y, M, D}};
get_initial_tokens(#yamerl_timestamp{year = Y, month = Mo, day = D,
                                     hour = H, minute = Mi, second = S}) ->
  {datetime, {{Y, Mo, D}, {H, Mi, S}}}.

construct(map, Pairs) ->
  Generator = fun({K, V}) -> {get_initial_tokens(K), get_initial_tokens(V)} end,
  construct(map, Pairs, Generator);
construct(seq, Elements) ->
  Generator = fun(E) -> get_initial_tokens(E) end,
  construct(seq, Elements, Generator).

construct(Type, Collection, Generator) ->
  Data =
    lists:foldl(
      fun(Element, Accu) ->
        append(Type, Accu, Generator(Element))
      end, construct(Type), Collection),
  constructed(Type, Data).

construct(map) -> #{};
construct(seq) -> [].

append(map, Map, {Key, Value}) -> Map#{Key => Value};
append(seq, Seq, Value) -> [Value | Seq].

constructed(map, Map) -> Map;
constructed(seq, Seq) -> lists:reverse(Seq).

convert_tokens(PropertySpec, {map, Map}, Path) ->
  maps:fold(
    fun(K, V, Accu) ->
      Key = convert_tokens(PropertySpec, K, Path),
      Accu#{Key => convert_tokens(PropertySpec, V, [Key | Path])}
    end, #{}, Map);
convert_tokens(PropertySpec, {seq, Seq}, Path) ->
  lists:reverse(lists:foldl(
    fun(E, Accu) ->
      [convert_tokens(PropertySpec, E, Path) | Accu]
    end, [], Seq));
convert_tokens(PropertySpec, SimpleToken, Path) ->
  convert(SimpleToken, lists:reverse(Path), PropertySpec).

convert({string, Value}, Path, PropertySpec) ->
  case is_property(Path, Value, PropertySpec) of
    true ->
      binary_to_atom(Value);
    false ->
      Value
  end;
convert({null, _}, _Path, _PropertySpec) -> null;
convert({bool, Bool}, _Path, _PropertySpec) -> Bool;
convert({int, Int}, _Path, _PropertySpec) -> Int;
convert({float, Float}, _Path, _PropertySpec) -> Float;
convert({binary, Binary}, _Path, _PropertySpec) -> Binary;
convert({bit_string, BitString}, _Path, _PropertySpec) -> BitString;
convert(Token, Path, _PropertySpec) -> throw({error, {invalid_token, Path, Token}}).

is_property(_, <<"$ref">>, _) ->
  true;
is_property(Path, Value, PropertySpec) ->
  PathToObject = get_obj_spec(Path, PropertySpec, PropertySpec),
  case PathToObject of
    none ->
      false;
    scalar ->
      false;
    atom ->
      true;
    _ ->
      maps:is_key(Value, PathToObject)
  end.

get_obj_spec([], ObjectSpec, _) ->
  ObjectSpec;
get_obj_spec(['$ref'], _, _) ->
  scalar;
get_obj_spec([E], ObjectSpec, PropertySpec) when is_atom(E) ->
  Spec = maps:get(atom_to_binary(E), ObjectSpec),
  resolve_obj_spec(Spec, PropertySpec);
get_obj_spec([_], scalar, _PropertySpec) ->
  scalar;
get_obj_spec([_], ObjectSpec, PropertySpec) ->
  case maps:get('_', ObjectSpec, undefined) of
    undefined ->
      none;
    Spec ->
      resolve_obj_spec(Spec, PropertySpec)
  end;
get_obj_spec([E | Rest], ObjectSpec, PropertySpec) when is_atom(E) ->
  Spec = maps:get(atom_to_binary(E), ObjectSpec),
  get_obj_spec(Rest, resolve_obj_spec(Spec, PropertySpec), PropertySpec);
get_obj_spec([_ | _Rest], scalar, _PropertySpec) ->
  scalar;
get_obj_spec([_ | Rest], ObjectSpec, PropertySpec) ->
  case maps:get('_', ObjectSpec, undefined) of
    undefined ->
      scalar;
    Spec0 ->
      Spec = resolve_obj_spec(Spec0, PropertySpec),
      get_obj_spec(Rest, Spec, PropertySpec)
  end.

resolve_obj_spec(scalar, _) ->
  scalar;
resolve_obj_spec(atom, _) ->
  atom;
resolve_obj_spec(#{} = ObjSpec, _) ->
  ObjSpec;
resolve_obj_spec(Ref, PropertySpec) ->
  resolve_obj_spec(maps:get(Ref, PropertySpec), PropertySpec).

generate_trails(HandlerRef, Server, Instance) ->
  maps:fold(
    fun(Path, MetaData, Accu) ->
      BaseOpts =
        #{path => Path,
          object => undefined,
          verbose => true},
      HandlerModule =
        case HandlerRef of
          {ref, Ref} -> binary_to_atom(maps:get(atom_to_binary(Ref), MetaData));
          {mod, Mod} -> Mod
        end,
      Opts =
        case Server of
          undefined -> BaseOpts;
          _ -> BaseOpts#{server => Server}
        end,
      Trail = trails:trail(Path, HandlerModule, Opts, MetaData),
      [Trail | Accu]
    end, [], maps:get(paths, Instance)).

save_to_global_spec(Instance) ->
  Default = #{info => #{title => <<"API-DOCS">>}},
  PrevGlobalSpec = application:get_env(cowboy_swagger, global_spec, Default),
  MergedGlobalSpec = merge(PrevGlobalSpec, Instance),
  application:set_env(cowboy_swagger, global_spec, MergedGlobalSpec).

merge(SpecOld, SpecNew) ->
  Spec0 = maps:merge(SpecOld, maps:without(?DEEP_MERGED_OBJECTS, SpecNew)),
  deep_merge(Spec0, SpecNew, ?DEEP_MERGED_OBJECTS, []).

deep_merge(To0, From, Keys, Path) ->
  {To, _, _} = lists:foldl(fun deep_merge_key/2, {To0, From, Path}, Keys),
  To.

deep_merge_key(Key, {To, From, Path}) ->
  case {maps:get(Key, To, undefined), maps:get(Key, From, undefined)} of
    {_, undefined} ->
      {To, From, Path};
    {undefined, New} ->
      {To#{Key => New}, From, Path};
    {Old, New} ->
      {To#{Key := do_merge(Old, New, [Key | Path])}, From, Path}
  end.

do_merge(#{} = To, #{} = From, Path) ->
  deep_merge(To, From, maps:keys(From), Path);
do_merge(To, From, _) when is_list(To) andalso is_list(From) ->
  lists:subtract(To, From) ++ From;
do_merge(V, V, _) ->
  V;
do_merge(V1, V2, Path) ->
  throw({error, {same_object_different_values, {lists:reverse(Path), {V1, V2}}}}).
