%%%-------------------------------------------------------------------
%% @doc geminic top level supervisor.
%% @author A. G. Madi (WarpEngineer@users.noreply.github.com)
%% @end
%%%-------------------------------------------------------------------

-module(geminic_sup).
-author( "agmadi" ).
-vsn( "0.1.0" ).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    SupFlags = #{strategy => one_for_all,
                 intensity => 1,
                 period => 5},
    ChildSpecs = [
		  #{id => geminic,
		    start => {geminic, start_link, []}
		   }
		 ],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
