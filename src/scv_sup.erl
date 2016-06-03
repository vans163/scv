-module(scv_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() -> supervisor:start_link({global, scv_sup}, ?MODULE, []).

init([]) ->
    AppSpec = {scv_gen, {scv_gen, start_link, []}, 
        temporary, 2000, worker, [scv_gen]
    },
    StartSpec = {simple_one_for_one, 0, 1},
    {ok, {StartSpec, [AppSpec]}}.