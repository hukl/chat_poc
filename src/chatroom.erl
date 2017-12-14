-module(chatroom).

-behaviour(gen_server).

-export([start_link/2]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(chat, {users, messages=[]}).

start_link(Name, User) ->
    gen_server:start_link(?MODULE, [Name, User], []).

init([Name, User]) ->
    History = find_or_create_history(Name),
    error_logger:info_msg("HISTORY ~p~n", [History]),
    ets:insert(chatrooms, {Name, self()}),
    State = #chat{users=sets:from_list([User])},
    tick(),
    {ok, State}.

handle_call({add_user, User}, _From, State) ->
    {reply, ok, State#chat{ users = sets:add_element(User, State#chat.users )}};


handle_call({message, User, Msg}, _From, State) ->
    NewState = State#chat{messages = [Msg|State#chat.messages]},

    Receivers = sets:del_element(User, State#chat.users),

    case sets:to_list(Receivers) of
        [] ->
            noop;
        [Receiver] ->
            send_to_other_user(User, Receiver, Msg)
    end,

    error_logger:info_msg("-STATE: ~p~n", [NewState]),
    {reply, ok, NewState};

handle_call(Request, _From, State) ->
    error_logger:info_msg("----------------------~p~n", [Request]),
    {reply, ok, State}.

handle_cast(Msg, State) ->
    error_logger:info_msg("MSG ~p~n", [Msg]),
    {noreply, State}.

handle_info(sync, State) ->
    % error_logger:info_msg("SYNCING TO FILE"),
    tick(),
    {noreply, State};

handle_info(Info, State) ->
    error_logger:info_msg("IIIIINFO ~p~n", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions


tick() ->
    erlang:send_after(5000, self(), sync).

find_or_create_history(Name) ->
    ok.

send_to_other_user(User, Receiver, Msg) ->
    error_logger:info_msg("WTF ~p ----- ~p ----- ~p~n", [User, Receiver, Msg]),

    case ets:lookup(users, Receiver) of
        [{_Name, Pid}] -> Pid ! {text, << User/binary, Msg/binary >>};
        Result         -> error_logger:info_msg("User not Online ~p ~p ~p", [User, Receiver, Result])
    end.
