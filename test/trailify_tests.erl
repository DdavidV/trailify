-module(trailify_tests).
-include_lib("eunit/include/eunit.hrl").

trailify_no_file_test() ->
  ?assertMatch({yamerl_exception, _}, trailify:open_api_to_trails("location")).

trailify_test() ->
  application:ensure_all_started(trailify),
  {ok, Instance} =
    trailify:open_api_to_trails(get_file_full_path("test.yaml"), #{generate_trails => false}),
  ?assertEqual(Instance,
               #{openapi => <<"3.0.0">>,
                 info => #{version => <<"0.0.1">>,
                           title => <<"Trailify">>},
                 paths => #{<<"/endpoint/:id">> =>
                            #{get =>
                               #{parameters => [#{'$ref' => <<"#/components/parameters/id">>}],
                                 responses => #{<<"200">> =>
                                                #{description => <<"Successful operation">>,
                                                  content => #{<<"application/json">> =>
                                                               #{schema => #{type => <<"object">>}}
                                                }}}},
                              put =>
                               #{parameters => [#{'$ref' => <<"#/components/parameters/id">>}],
                                 responses => #{<<"200">> =>
                                                #{description => <<"Successful operation">>,
                                                  content => #{<<"application/json">> =>
                                                               #{schema => #{type => <<"object">>}}
                                                }}},
                                 'requestBody' => #{'$ref' =>
                                                    <<"#/components/requestBodies/ReqBody">>}},
                              delete =>
                               #{parameters => [#{'$ref' => <<"#/components/parameters/id">>}],
                                 responses => #{<<"200">> =>
                                                #{description => <<"Successful operation">>,
                                                  content => #{<<"application/json">> =>
                                                                #{schema => #{type => <<"object">>}}
                                                }}}},
                              post =>
                               #{parameters => [#{'$ref' => <<"#/components/parameters/id">>}],
                                 responses => #{<<"200">> =>
                                                #{description => <<"Successful operation">>,
                                                  content => #{<<"application/json">> =>
                                                                #{schema => #{type => <<"object">>}}
                                                }}},
                                 'requestBody' => #{'$ref' =>
                                                    <<"#/components/requestBodies/ReqBody">>}}}},
                 components =>
                   #{parameters => #{<<"id">> =>
                                     #{in => <<"path">>,
                                       name => <<"id">>,
                                       schema => #{type => <<"integer">>,
                                                   format => <<"int64">>},
                                       required => true}},
                     'requestBodies' => #{<<"ReqBody">> =>
                                          #{content =>
                                            #{<<"application/json">> =>
                                              #{schema =>
                                                #{'$ref' =>
                                                  <<"#/components/schemas/Schema">>}}}}},
                     schemas => #{<<"Schema">> =>
                                  #{type => <<"object">>,
                                    required => [<<"message">>,
                                                 <<"object">>],
                                    properties => #{<<"message">> =>
                                                    #{type => <<"string">>,
                                                      example => <<"message">>},
                                  <<"object">> =>
                                  #{type => <<"object">>,
                                    properties => #{<<"data">> =>
                                                    #{type => <<"string">>,
                                                      example => <<"data_type">>},
                                                    <<"data_list">> =>
                                                    #{type => <<"array">>,
                                                      items =>
                                                        #{type => <<"string">>,
                                                          example =>
                                                          <<"[data_1, data_2]">>}}}}}}}}
      }).


trailify_generate_trails_test() ->
  application:ensure_all_started(trailify),
  {ok, Trails} = trailify:open_api_to_trails(get_file_full_path("small_with_x-handler.yaml")),
  ?assertEqual(Trails,
               [#{options => #{path => <<"/endpoint">>,
                               verbose => true,
                               object => undefined},
                  handler => trailify_handler,
                  metadata =>
                    #{get => #{responses =>
                                #{<<"200">> =>
                                  #{description => <<"Successful operation">>,
                                    content =>
                                      #{<<"application/json">> =>
                                         #{schema =>
                                           #{type => <<"object">>}}}}}},
                      <<"x-handler">> => <<"trailify_handler">>},
                  constraints => [],
                  path_match => <<"/endpoint">>}]).

trailify_generate_trails_not_default_test() ->
  application:ensure_all_started(trailify),
  {ok, Trails} =
    trailify:open_api_to_trails(get_file_full_path("small_with_another_handler_ref.yaml"),
                                #{generate_trails => true,
                                  handler => {yaml, 'x-another-handler'}}),
  ?assertEqual(Trails,
               [#{options => #{path => <<"/endpoint">>,
                               verbose => true,
                               object => undefined},
                  handler => another_handler,
                  metadata =>
                    #{get => #{responses =>
                                #{<<"200">> =>
                                  #{description => <<"Successful operation">>,
                                    content =>
                                      #{<<"application/json">> =>
                                         #{schema =>
                                           #{type => <<"object">>}}}}}},
                      <<"x-another-handler">> => <<"another_handler">>},
                  constraints => [],
                  path_match => <<"/endpoint">>}]).

get_file_full_path(File) ->
  filename:join([code:lib_dir(trailify), "test", "test_yaml", File]).