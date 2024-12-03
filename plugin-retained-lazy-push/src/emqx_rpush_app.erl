-module(emqx_rpush_app).

-behaviour(application).

-emqx_plugin(?MODULE).

-export([
    start/2,
    stop/1
]).

start(_StartType, _StartArgs) ->
    emqx_rpush:load(),
    {ok, Sup} = emqx_rpush_sup:start_link(),
    emqx_ctl:register_command(emqx_rpush, {emqx_rpush_cli, cmd}),
    {ok, Sup}.

stop(_State) ->
    emqx_ctl:unregister_command(emqx_rpush),
    emqx_rpush:unload(),
    ok.
