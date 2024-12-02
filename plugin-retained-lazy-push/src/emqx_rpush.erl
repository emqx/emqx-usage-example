-module(emqx_rpush).

%% for #message{} record
%% no need for this include if we call emqx_message:to_map/1 to convert it to a map
-include_lib("emqx/include/emqx.hrl").
-include_lib("emqx/include/emqx_hooks.hrl").

%% for logging
-include_lib("emqx/include/logger.hrl").

-export([
    load/1,
    unload/0
]).

%% Message Pubsub Hooks
-export([on_message_publish/2]).

%% Called when the plugin application start
load(Env) ->
    hook('message.publish', {?MODULE, on_message_publish, [Env]}).

on_message_publish(Message, _Env) ->
    ok = forward_to_peer_broker(emqx_message:to_map(Message)),
    {ok, Message}.

forward_to_peer_broker(#{topic := Topic} = Message) ->
    case is_match(Topic) of
        true ->
            emqx_rpush_client:publish(Message);
        false ->
            ok
    end.

%% Called when the plugin application stop
unload() ->
    unhook('message.publish', {?MODULE, on_message_publish}).

hook(HookPoint, MFA) ->
    %% use highest hook priority so this module's callbacks
    %% are evaluated before the default hooks in EMQX
    emqx_hooks:add(HookPoint, MFA, _Property = ?HP_HIGHEST).

unhook(HookPoint, MFA) ->
    emqx_hooks:del(HookPoint, MFA).

is_match(Topic) ->
    Filters = emqx_rpush_config:get_topics(),
    lists:any(
        fun(Filter) ->
            emqx_topic:match(Topic, Filter)
        end,
        Filters
    ).
