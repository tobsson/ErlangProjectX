-module(wordval_app).

%% Application callbacks
-export([init/0]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

init() ->
  wordval_sup:start_link(),
  couchbeam:start(),
	word_server:start_link(),
	resmanager_server:start_link().
