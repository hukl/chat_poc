%% Feel free to use, reuse and abuse the code in this file.

%% @private
-module(chat_experiment_sup).
-behaviour(supervisor).

%% API.
-export([start_link/0]).

%% supervisor.
-export([init/1]).


%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% supervisor.

init([]) ->
    Procs = [?CHILD(chatroom_sup, supervisor)],
    {ok, {{one_for_one, 10, 10}, Procs}}.
