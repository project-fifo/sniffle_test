-module(libchunter_intercepts).

-compile(export_all).
%%-include("intercept.hrl").
-define(M, libchunter_orig).

ping_ok(_Server, _Port) ->
    pong.

ping_fail(_Server, _Port) ->
    {error, connection_failed}.
