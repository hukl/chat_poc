%% Feel free to use, reuse and abuse the code in this file.

%% @private
-module(chat_experiment_app).
-behaviour(application).

%% API.
-export([start/2]).
-export([stop/1]).

%% API.
start(_Type, _Args) ->
    application:start(sasl),
    application:start(crypto),
    application:start(cowlib),
    application:start(ranch),
    application:start(cowboy),

    Dispatch = cowboy_router:compile([
        {'_', [
            {"/", cowboy_static, {priv_file, chat_experiment, "index.html"}},
            {"/chat_experiment", ws_handler, []},
            {"/static/[...]", cowboy_static, {priv_dir, chat_experiment, "static"}}
        ]}
    ]),
    {ok, _} = cowboy:start_clear(http, [{port, 8080}], #{
        env => #{dispatch => Dispatch}
    }),
    chat_experiment_sup:start_link().

stop(_State) ->
    ok.
