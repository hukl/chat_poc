-module(chatroom_sup).
-behaviour(supervisor).

%% API.
-export([start_link/0]).

%% supervisor.
-export([init/1, start_chatroom/1]).


%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% supervisor.

start_chatroom(Name) ->
    supervisor:start_child(?MODULE, [Name]).

init([]) ->
    % init active chatroom table
    ets:new(chatrooms, [set, named_table, public]),

    Chatroom = {
        chatroom,
        {chatroom, start_link, []},
        temporary, brutal_kill, worker, [chatroom]},
    RestartStrategy = {simple_one_for_one, 0, 1},
    {ok, {RestartStrategy, [Chatroom]}}.
