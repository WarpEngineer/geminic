%%%-------------------------------------------------------------------
%% @doc geminic public API
%% @author A. G. Madi (WarpEngineer@users.noreply.github.com)
%% @end
%%%-------------------------------------------------------------------

-module(geminic_app).
-author( "agmadi" ).
-vsn( "0.1.0" ).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    geminic_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
