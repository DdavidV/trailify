-module(trailify).

-export([ open_api_to_trails/1
        , open_api_to_trails/2]).

-spec open_api_to_trails(FileLocation) -> Result when
  FileLocation :: string(),
  Result :: {ok, trails:trail()} | {ok, trailify_open_api_converter:instance()}.
open_api_to_trails(FileLocation) ->
  open_api_to_trails(FileLocation, #{handler => {yaml, 'x-handler'},
                                     generate_trails => true}).

-spec open_api_to_trails(FileLocation, Options) -> Result when
  FileLocation :: string(),
  Options :: trailify_open_api_converter:options(),
  Result :: {ok, trails:trail()} | {ok, trailify_open_api_converter:instance()}.
open_api_to_trails(FileLocation, Options) ->
  trailify_open_api_converter:to_trails(FileLocation, Options).