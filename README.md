trailify
=====
trailify is a library that parses openAPI yaml swagger definitions into cowboy trails.
</br>
I suggest keeping your yamls under **priv/swagger**

# Usage
    
    -module(my_handler).
    -behaviour(trails_handler).
    -export([trails/0]).

    ...

    trails() ->
      FilePath = filename:join([code:priv_dir(?APP_NAME), "swagger" "my_endpoints.yaml"]),
      {ok, Trails} = trailify:open_api_to_trails(FilePath),
      Trails.

By default trailify will generate trails based on the **x-handler** attribute under your endpoint.
(For examples check the test_yaml folder)

# Configuring trailify
trailify has a 2 arity function: trailify:open_api_to_trails/2 where the second parameter is an
option map.

## Options
- handler: by default trailify will use **x-handler** attribute under your endpoint from the yaml.
You can use other attribute from the yaml by specifying: {yaml, Attribute} or you can pass your
handler module's name if you don't want to put an extra attribute into your yaml.
- server: this will be put under options as **server => Server**
- generate_trails: this option will tell trailify if trails generation is needed or not. If not
trailify will return the swagger instance. This could be useful to do some post processing or to
export your schemas for jesse (a library for validating json).
