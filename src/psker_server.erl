-module(psker_server).

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
    TlsFile = fun(Name) -> filename:join([Pwd, tls, Name]) end,
    io:format(user, "[server] pwd=~p~n", [Pwd]),
    {ok, ListenSock} =
    ssl:listen(psker:server_port(),
                   [{cacertfile, TlsFile("ca.pem")},
                    {certfile, TlsFile("server-chain.pem")},
                    {keyfile, TlsFile("server.key")},
                    {protocol, psker:protocol()},
                    {reuseaddr, true},
                    {verify, verify_peer},
                    {versions, psker:versions()},
                    {psk_identity, atom_to_list(name())},
                    {user_lookup_fun, {fun psker:lookup/3, #{}}},
                    {ciphers, psker:cipher_suites(server)},
                    {active, true},
                    {log_level, debug}
                   ]),
    {ok, _State = listening, _Data = #{listening => ListenSock}}.

callback_mode() ->
    [state_enter, handle_event_function].

handle_event(enter, _OldState, listening, #{listening := ListenSock} = D) ->
    io:format(user, "[server] listening~n", []),
    {ok, Socket0} = ssl:transport_accept(ListenSock),
    {ok, Socket} = ssl:handshake(Socket0),
    io:format(user, "[server] accpted one client~n~p~n", [ssl:negotiated_protocol(Socket)]),
    {next_state, listening, D, [{state_timeout, 0, {accepted, Socket}}]};
handle_event(state_timeout, {accepted, Sock}, listening, D) ->
    {next_state, accepted, D#{accepted => Sock}};
handle_event(info, {ssl_closed, Sock}, accepted, #{accepted := Sock} = D) ->
    ssl:close(Sock),
    {next_state, listening, D};
handle_event(EventType, Event, accepted, _Data) ->
    io:format(user, "[server] ignored event: ~p: ~0p~n", [EventType, Event]),
    keep_state_and_data.
