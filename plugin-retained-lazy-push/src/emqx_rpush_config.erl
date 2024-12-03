-module(emqx_rpush_config).

-export([load/0, unload/0, get_topics/0, mqtt_client_opts/0, forward_ns/0]).

get_topics() ->
    #{<<"topics">> := Topics} = get_config(),
    Topics.

mqtt_client_opts() ->
    #{<<"peer">> := Peer} = get_config(),
    #{
        <<"host">> := Host,
        <<"port">> := Port,
        <<"clientid_prefix">> := ClientIdPrefix,
        <<"username">> := Username,
        <<"password">> := Password
    } = Peer,
    #{
        host => Host,
        port => Port,
        clientid => clientid(ClientIdPrefix),
        username => Username,
        password => Password
    }.

forward_ns() ->
    #{<<"peer">> := #{<<"forward_ns">> := Ns}} = get_config(),
    Ns.

clientid(Prefix) ->
    iolist_to_binary([Prefix, atom_to_list(node())]).

get_config() ->
    case persistent_term:get(?MODULE, undefined) of
        undefined ->
            ok = load(),
            get_config();
        Conf ->
            Conf
    end.

load() ->
    ConfFile = filename:join([code:priv_dir(emqx_rpush), "config.hocon"]),
    case hocon:load(ConfFile) of
        {ok, Conf} ->
            persistent_term:put(?MODULE, Conf),
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

unload() ->
    persistent_term:erase(?MODULE).
