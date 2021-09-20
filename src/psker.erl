-module(psker).

-export([lookup/3, psk_suites/0]).

lookup(psk, PSKIdentity, UserState) ->
    io:format(user, "######################### looking up ~p: ~p~n", [PSKIdentity, UserState]),
    %% this should be read from a file (cache) or database
    {ok, <<"the-shared-secret">>}.

psk_suites() ->
    ["RSA-PSK-AES256-GCM-SHA384","RSA-PSK-AES256-CBC-SHA384",
     "RSA-PSK-AES128-GCM-SHA256","RSA-PSK-AES128-CBC-SHA256",
     "RSA-PSK-AES256-CBC-SHA","RSA-PSK-AES128-CBC-SHA",
     "RSA-PSK-DES-CBC3-SHA","RSA-PSK-RC4-SHA"
    ].
