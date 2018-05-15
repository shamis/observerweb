%%%-------------------------------------------------------------------
%%% @author Bill Wang
%%% @copyright (C) 2017, Freecnpro
%%% @doc
%%%
%%% @end
%%% Created : 2017-04-17
%%%-------------------------------------------------------------------
-module(observerweb_app).
-author("bill@freecnpro.net").

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

-spec(start(StartType :: normal | {takeover, node()} | {failover, node()}, StartArgs :: term()) ->
    {ok, pid()} |
    {ok, pid(), State :: term()} |
    {error, Reason :: term()}).
start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
        {'_', [
          {"/", cowboy_static, {priv_file, observerweb, "index.html"}},
          {"/css/[...]", cowboy_static, {priv_dir, observerweb, "css"}},
          {"/bower_components/[...]", cowboy_static, {priv_dir, observerweb, "bower_components"}},
          {"/public/[...]", cowboy_static, {priv_dir, observerweb, "public"}},
          {"/img/[...]", cowboy_static, {priv_dir, observerweb, "img"}},
          {"/info", observerweb_handler, []}
        ]}
    ]),

    NbAcceptors = observerweb:env(acceptors, 10),
    Port = observerweb:env(port, 8449),

    {ok, _} = cowboy:start_http(http, NbAcceptors, [{port, Port}], [
        {env, [{dispatch, Dispatch}]}
    ]),
    observerweb_sup:start_link().

-spec(stop(State :: term()) -> term()).
stop(_State) ->
    ok.
