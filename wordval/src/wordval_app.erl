-module(wordval_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    wordval_sup:start_link(),
    couchbeam:start(),
	word_server:start_link().

stop(_State) ->
    ok.
