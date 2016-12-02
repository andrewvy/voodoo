%%%-------------------------------------------------------------------
%% @doc voodoo top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(voodoo_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    {ok, {{one_for_all, 0, 1}, [
            {voodoo_vegur, {vegur, start_http, [8080, voodoo_backend, [{middlewares, vegur:default_middlewares()}]]}, permanent, 5000, worker, [vegur]}
          ]}}.

%%====================================================================
%% Internal functions
%%====================================================================
