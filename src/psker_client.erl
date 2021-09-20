-module(psker_client).

-behaviour(gen_statem).

-export([start_link/0, stop/0]).
-export([terminate/3,
         code_change/4,
         init/1,
         callback_mode/0,
         handle_event/4
        ]).

name() -> ?MODULE.

start_link() ->
    application:ensure_all_started(ssl),
    gen_statem:start_link({local, name()}, ?MODULE, [], []).

stop() ->
    gen_statem:stop(name()).

terminate(_Reason, _State, _Data) ->
    void.

code_change(_Vsn, State, Data, _Extra) ->
    {ok, State, Data}.

init([]) ->
    {ok, Pwd} = file:get_cwd(),
    io:format(user, "[client] pwd=~p~n", [Pwd]),
    TlsFile = fun(Name) -> filename:join([Pwd, tls, Name]) end,
    Opts = [{cacertfile, TlsFile("ca.pem")},
            {certfile, TlsFile("client.pem")},
            {keyfile, TlsFile("client.key")},
            {verify, verify_peer},
            {versions, ['tlsv1.2', 'tlsv1.1']}, %% can not use tlsv1.3 for psk
            {psk_identity, atom_to_list(name())},
            {user_lookup_fun, {fun psker:lookup/3, #{}}},
            {ciphers, psker:cipher_suites(client)},
            {log_level, debug}
           ],
    {ok, Socket} = ssl:connect("localhost", 9999, Opts, infinity),
    io:format(user, "[client] connected to server~n", []),
    {ok, _State = connected, _Data = #{socket => Socket},
     [{state_timeout, 100, send}]}.

callback_mode() ->
    handle_event_function.


handle_event(state_timeout, send, _State, #{socket := Socket}) ->
    ssl:send(Socket, "hey"),
    {keep_state_and_data, [{state_timeout, 5000, send}]};
handle_event(EventType, Event, _State, _Data) ->
    io:format(user, "[client] ignored event: ~p: ~p~n", [EventType, Event]),
    keep_state_and_data.
