-module(emqx_rpush_client).

-behaviour(gen_server).

-export([publish/1]).
-export([start_link/0]).
-export([reply_publish_result/4]).
-export([
    init/1,
    handle_continue/2,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
]).

-define(WORKER, emqx_rpush_worker).
-define(CLIENT, emqx_rpush_client).
%% the queue of messages to be published
-define(BUFFER, emqx_rpush_buffer).
%% the index of messages from topic to message id for retained messages
%% A retained message is indexed from topic to id
%% When a client (the hook caller) needs to publish a retained message
%% it inserts to the buffer and the index
%% When publishing a retained message, the client process should check if
%% there next(Index, {Topic, Id}) has the same topic,
%% if so, the retained message should be ignored, and the index should be deleted.
-define(INDEX, emqx_rpush_index).

-define(MAX_INFLIGHT, 20).
-define(RECONNECT_INTERVAL, 5000).

-define(LOG(Level, Msg), logger:log(Level, Msg, #{domain => [emqx_rpush]})).
-define(SMALLEST_ID, 0).

start_link() ->
    gen_server:start_link({local, ?CLIENT}, ?MODULE, [], []).

publish(Msg) ->
    #{
        topic := Topic,
        flags := Flags
    } = Msg,
    Id = generate_id(),
    case is_retainned(Flags) of
        true ->
            ets:insert(?INDEX, {{Topic, Id}, []});
        false ->
            ok
    end,
    ets:insert(?BUFFER, {Id, Msg}),
    erlang:send(?CLIENT, poll_now),
    ok.

init(_Args) ->
    process_flag(trap_exit, true),
    {ok, Buffer} = new_tab(?BUFFER),
    {ok, Index} = new_tab(?INDEX),
    State = #{
        buffer => Buffer,
        index => Index,
        last_id => ?SMALLEST_ID,
        inflight => 0,
        reconnect_tref => undefined,
        forward_ns => emqx_rpush_config:forward_ns()
    },
    {ok, State, {continue, connect}}.

handle_continue(connect, State) ->
    case start_link_mqtt_client() of
        {ok, Pid} ->
            ok = signal_self_to_poll(),
            {noreply, State#{worker => Pid}};
        error ->
            {noreply, ensure_reconnect(State)}
    end.

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_call}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(poll_now, State) ->
    poll_now(State);
handle_info({publish_result, Id, Topic, Result}, State) ->
    #{inflight := Inflight, buffer := Buffer, index := Index} = State,
    case handle_publish_result(Id, Topic, Result, Buffer, Index) of
        {ok, Ack} ->
            ?LOG(info, #{msg => "publish_ack", ack => Ack}),
            poll_now(State#{inflight => Inflight - 1});
        {error, Reason} ->
            ?LOG(error, #{msg => "publish_error", reason => Reason}),
            {noreply, rewind(State)}
    end;
handle_info(reconnect, #{reconnect_tref := Ref} = State) ->
    _ = erlang:cancel_timer(Ref),
    handle_continue(connect, State#{reconnect_tref => undefined});
handle_info({'EXIT', Pid, Reason}, #{worker := Pid} = State) ->
    ?LOG(error, #{msg => "worker_down", reason => Reason}),
    {noreply, rewind(State)};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

poll_now(#{worker := Worker} = State) when is_pid(Worker) ->
    #{
        worker := Worker,
        buffer := Buffer,
        index := Index,
        last_id := LastId,
        inflight := Inflight,
        forward_ns := Ns
    } = State,
    Now = now_ts(),
    Max = ?MAX_INFLIGHT - Inflight,
    {Count, NewLastId} = send_from_buffer(Buffer, Index, Worker, Ns, LastId, Now, Max, 0),
    {noreply, State#{last_id => NewLastId, inflight => Inflight + Count}};
poll_now(State) ->
    %% worker is down, ensure reconnect timer
    {noreply, ensure_reconnect(State)}.

ensure_reconnect(#{reconnect_tref := Tref} = State) when is_reference(Tref) ->
    State;
ensure_reconnect(#{reconnect_tref := undefined} = State) ->
    Tref = erlang:send_after(?RECONNECT_INTERVAL, self(), reconnect),
    State#{reconnect_tref => Tref}.

start_link_mqtt_client() ->
    EmqttOpts0 = emqx_rpush_config:mqtt_client_opts(),
    %% do not reconnect by itself, owner will handle reconnect
    EmqttOpts = EmqttOpts0#{reconnect => false},
    {ok, Pid} = emqtt:start_link(EmqttOpts),
    case emqtt_connect(Pid) of
        {ok, Properties} ->
            ?LOG(info, #{msg => "mqtt_client_connected", properties => Properties}),
            register(?WORKER, Pid),
            {ok, Pid};
        {error, Reason} ->
            ?LOG(info, #{msg => "mqtt_client_connect_error", reason => Reason}),
            error
    end.

emqtt_connect(Pid) ->
    try
        emqtt:connect(Pid)
    catch
        exit:Reason ->
            {error, Reason}
    end.

new_tab(Name) ->
    Tab = ets:new(Name, [
        % make it easier to debug, should not be named when pooled
        named_table,
        ordered_set,
        public,
        {write_concurrency, true}
    ]),
    {ok, Tab}.

now_ts() ->
    erlang:system_time(millisecond).

send_from_buffer(_Buffer, _Index, _Worker, _Ns, LastId, _NowTs, Max, Count) when Count >= Max ->
    {Count, LastId};
send_from_buffer(Buffer, Index, Worker, Ns, LastId, NowTs, Max, Count) ->
    case ets:next(Buffer, LastId) of
        '$end_of_table' ->
            {Count, LastId};
        Id ->
            [{_, #{topic := Topic} = Msg}] = ets:lookup(Buffer, Id),
            case is_stale(Index, Id, Msg, NowTs) of
                true ->
                    delete_msg(Buffer, Index, Id, Topic),
                    send_from_buffer(Buffer, Index, Worker, Ns, Id, NowTs, Max, Count);
                false ->
                    ok = do_publish(Worker, Ns, Id, Msg),
                    send_from_buffer(Buffer, Index, Worker, Ns, Id, NowTs, Max, Count + 1)
            end
    end.

%% a message is stale if it is expired or there is a newer retained message
is_stale(Index, Id, #{topic := Topic, flags := Flags} = Msg, NowTs) ->
    is_expired(Msg, NowTs) orelse
        (is_retainned(Flags) andalso has_newer_retained(Index, Topic, Id)).

is_retainned(#{retain := true}) ->
    true;
is_retainned(_) ->
    false.

has_newer_retained(Index, Topic, Id) ->
    case ets:next(Index, {Topic, Id}) of
        {T, _} ->
            T =:= Topic;
        _ ->
            false
    end.

is_expired(#{timestamp := Timestamp, headers := Headers}, NowTs) ->
    Properties = maps:get(properties, Headers, #{}),
    ExpireT = expire_time(Timestamp, Properties),
    is_integer(ExpireT) andalso ExpireT < NowTs.

expire_time(Timestamp, #{'Message-Expiry-Interval' := Seconds}) ->
    Timestamp + timer:seconds(Seconds);
expire_time(_, _) ->
    infinity.

do_publish(Worker, Ns, Id, #{payload := Payload, topic := Topic, qos := QoS}) ->
    Callback = {fun ?MODULE:reply_publish_result/4, [self(), Id, Topic]},
    NsTopic = iolist_to_binary([Ns, Topic]),
    ok = emqtt:publish_async(Worker, NsTopic, Payload, QoS, Callback).

reply_publish_result(Client, Id, Topic, Result) ->
    erlang:send(Client, {publish_result, Id, Topic, Result}).

handle_publish_result(Id, Topic, ok, Buffer, Index) ->
    ok = delete_msg(Buffer, Index, Id, Topic),
    {ok, noack};
handle_publish_result(Id, Topic, {ok, Ack}, Buffer, Index) ->
    ok = delete_msg(Buffer, Index, Id, Topic),
    {ok, Ack};
handle_publish_result(_Id, _Topic, {error, Reason}, _Buffer, _Index) ->
    {error, Reason}.

delete_msg(Buffer, Index, Id, Topic) ->
    ets:delete(Index, {Topic, Id}),
    ets:delete(Buffer, Id),
    ok.

shutdown_worker(undefined) ->
    ok;
shutdown_worker(Worker) ->
    monitor(process, Worker),
    unlink(Worker),
    exit(Worker, shutdown),
    receive
        {'DOWN', _, process, Worker, _Reason} ->
            ok;
        {'EXIT', Worker, _Reason} ->
            ok
    after 1000 ->
        ?LOG(error, #{msg => "worker_shutdown_timeout"}),
        exit(Worker, kill),
        receive
            {'DOWN', _, process, Worker, _Whatever} ->
                ok
        end
    end.

rewind(#{worker := Worker} = State) ->
    ok = shutdown_worker(Worker),
    ensure_reconnect(State#{
        worker => undefined,
        inflight => 0,
        last_id => ?SMALLEST_ID
    }).

%% Generate unique ID for each message in the same buffer queue.
%% The ID has to be monitonically increasing to ensure the client process will not miss any message
%% because it iterates the buffer queue from the last ID.
%% TODO: For better concurrency, use a pool of buffers and workers)
%% and generate ID from ets:update_counter/3
%% e.g. ets:update_counter(?ID_TABLE, PartitionNumber, 1, 1)
generate_id() ->
    erlang:unique_integer([positive, monotonic]).

signal_self_to_poll() ->
    erlang:send(self(), poll_now),
    ok.
