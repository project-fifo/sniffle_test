-module(hackney_intercepts).

-compile(export_all).
-include("intercept.hrl").
-define(M, hackney_orig).

request_404(_Method, _URL, _Header, _Body, _Opts) ->
    {ok, 404, undefined, undefined}.
