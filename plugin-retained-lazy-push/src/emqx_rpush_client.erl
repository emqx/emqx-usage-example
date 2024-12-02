-module(emqx_rpush_client).

-export([publish/1]).
-export([start_link/0]).

-define(CLIENT, emqtt_rpush_client).

start_link() ->
    EmqttOpts = emqx_rpush_config:mqtt_client_opts(),
    {ok, Pid} = emqtt:start_link(EmqttOpts),
    {ok, _Properties} = emqtt:connect(Pid),
    register(?CLIENT, Pid),
    {ok, Pid}.

publish(#{topic := Topic, payload := Payload}) ->
    NewTopic = <<"forward/", Topic/binary>>,
    io:format(user, "Forwarding message to ~s~n", [NewTopic]),
    ok = emqtt:publish(?CLIENT, NewTopic, Payload).
