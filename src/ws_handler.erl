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
    {reply, {text, << "Logged in as ", Username/binary >>}, State};

websocket_handle({text, Msg}, State) ->
    Chatroom = case ets:lookup(chatrooms, ?CHATROOM) of
        [] ->
            error_logger:info_msg("NO CHATROOM", []),
            {ok, Pid} = chatroom_sup:start_chatroom(?CHATROOM),
            Pid;
        [{_ChatroomName, Pid}] ->
            error_logger:info_msg("Found CHATROOM ~p~n", [Pid]),
            Pid
    end,

    gen_server:call(Chatroom, {message, Msg}),
    {reply, {text, << "That's what she said! ", Msg/binary >>}, State};
websocket_handle(_Data, State) ->
    {ok, State}.

websocket_info({timeout, _Ref, Msg}, State) ->
    erlang:start_timer(1000, self(), <<"How' you doin'?">>),
    {reply, {text, Msg}, State};
websocket_info(_Info, State) ->
    {ok, State}.
