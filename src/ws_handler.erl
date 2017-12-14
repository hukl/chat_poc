-module(ws_handler).

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).

-define(CHATROOM, peter).

init(Req, Opts) ->
    {cowboy_websocket, Req, Opts}.

websocket_init(State) ->
    erlang:start_timer(1000, self(), <<"Hello!">>),
    {ok, State}.

websocket_handle({text, <<"--heartbeat--">>}, State) ->
    error_logger:info_msg("~p~n", [State]),
    {reply, {text, <<"--pong--">>}, State};

websocket_handle({text, << "login:", Username/binary >>}, State) ->
    ets:insert(users, {Username, self()}),
    {reply, {text, << "Logged in as ", Username/binary >>}, State};

websocket_handle({text, <<User:4/binary, Msg/binary>> = Message}, State) ->
    Chatroom = case ets:lookup(chatrooms, ?CHATROOM) of
        [] ->
            error_logger:info_msg("NO CHATROOM", []),
            {ok, Pid} = chatroom_sup:start_chatroom(?CHATROOM, User),
            Pid;
        [{_ChatroomName, Pid}] ->
            error_logger:info_msg("Found CHATROOM ~p~n", [Pid]),
            gen_server:call(Pid, {add_user, User}),
            Pid
    end,

    error_logger:info_msg("UUUUSER ~p Message~p~n", [User, Msg]),
    gen_server:call(Chatroom, {message, User, Msg}),
    {reply, {text, Message}, State};

websocket_handle(Data, State) ->
    error_logger:info_msg("DAAATA ~p~n", [Data]),
    {ok, State}.

websocket_info({text, Msg}, State) ->
    {reply, {text, Msg}, State};

websocket_info(_Data, State) ->
    {ok, State}.
