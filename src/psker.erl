-module(psker).

-export([lookup/3, cipher_suites/1, versions/0, protocol/0, server_port/0]).

lookup(psk, PSKIdentity, UserState) ->
    io:format(user, "######################### looking up ~p: ~p~n", [PSKIdentity, UserState]),
    %% this should be read from a file (cache) or database
    {ok, <<"the-shared-secret">>}.

cipher_suites(server) ->
    ssl:cipher_suites(all, 'tlsv1.2', openssl) ++ non_rsa_ciphers();
cipher_suites(client) ->
    case os:getenv("PSKER_CIPHER") of
        false -> rsa_ciphers() ++ non_rsa_ciphers();
        "non-rsa" -> non_rsa_ciphers();
        "rsa" -> rsa_ciphers();
        Cipher -> [Cipher]
    end.

rsa_ciphers() ->
    ["RSA-PSK-AES256-GCM-SHA384","RSA-PSK-AES256-CBC-SHA384",
     "RSA-PSK-AES128-GCM-SHA256","RSA-PSK-AES128-CBC-SHA256",
     "RSA-PSK-AES256-CBC-SHA","RSA-PSK-AES128-CBC-SHA",
     "RSA-PSK-DES-CBC3-SHA","RSA-PSK-RC4-SHA"
    ].

non_rsa_ciphers() ->
    ["PSK-AES256-GCM-SHA384",
     "PSK-AES128-GCM-SHA256",
     "PSK-AES256-CBC-SHA384",
     "PSK-AES256-CBC-SHA",
     "PSK-AES128-CBC-SHA256",
     "PSK-AES128-CBC-SHA"
    ].

protocol() ->
    case os:getenv("PSKER_PROTOCOL") of
        "dtls" -> dtls;
        _ -> tls
    end.

versions() -> versions(protocol()).

versions(dtls) -> ['dtlsv1.2'];
versions(tls) -> ['tlsv1.2', 'tlsv1.1']. %% can not use tlsv1.3 for psk

server_port() ->
    case os:getenv("PSKER_SERVER_PORT") of
        false -> 9999;
        N -> list_to_integer(N)
    end.
