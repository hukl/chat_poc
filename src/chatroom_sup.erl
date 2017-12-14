-module(chatroom_sup).
-behaviour(supervisor).

%% API.
-export([start_link/0]).

%% supervisor.
-export([init/1, start_chatroom/2]).


%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% API.

-spec start_link() -> {ok, pid()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% supervisor.

start_chatroom(Name, User) ->
    supervisor:start_child(?MODULE, [Name, User]).

init([]) ->
    % init active chatroom table
    ets:new(chatrooms, [set, named_table, public]),
    ets:new(users,     [set, named_table, public]),

    Chatroom = {
        chatroom,
        {chatroom, start_link, []},
        temporary, brutal_kill, worker, [chatroom]},
    RestartStrategy = {simple_one_for_one, 0, 1},
    {ok, {RestartStrategy, [Chatroom]}}.
