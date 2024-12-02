-module(emqx_rpush_app).

-behaviour(application).

-emqx_plugin(?MODULE).

-export([
    start/2,
    stop/1
]).

start(_StartType, _StartArgs) ->
    {ok, Sup} = emqx_rpush_sup:start_link(),
    emqx_rpush:load(application:get_all_env()),
    emqx_ctl:register_command(emqx_rpush, {emqx_rpush_cli, cmd}),
    {ok, Sup}.

stop(_State) ->
    emqx_ctl:unregister_command(emqx_rpush),
    emqx_rpush:unload(),
    emqx_rpush_config:unload(),
    ok.
