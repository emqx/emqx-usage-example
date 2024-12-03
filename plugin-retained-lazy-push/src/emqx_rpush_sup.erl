-module(emqx_rpush_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Children = [
        #{
            id => emqx_rpush_client,
            start => {emqx_rpush_client, start_link, []},
            type => worker,
            restart => transient,
            shutdown => 5000
        }
    ],
    {ok, {#{strategy => one_for_one}, Children}}.
