-module(resmanager_server).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% NOTE: This module needs Couchbeam to be available to work with CouchDb database

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, store_res/1, get_res/1, get_wordscore/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

%Starts up the gen_server
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%Function to evaluate 1 word, takes a list in the form: [Word,Neutral, Negative, Positive]
store_res(List) ->
    gen_server:call(?SERVER,{storeres, List},infinity).
	
%Function to get results stored in DB for a searched subject, takes a a searchterm in the form: <<"searchterm">>
get_res(Word) ->
    gen_server:call(?SERVER,{getres, Word},infinity).
	
%Function to get score for a word stored in wordlist DB for a searched subject
get_wordscore(Word) ->
    gen_server:call(?SERVER,{wordscore, Word},infinity).
	
	
%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

%Starts couchbeam upon gen_server startup and establishes a connection to our database for wordlist
init([]) ->
    couchbeam:start(),
	Url = "localhost:5984",
    Options = [],
    S = couchbeam:server_connection(Url, Options),  % connect to the server
    %%{ok, Db}=couchbeam:open_db(S, "results", Options), % opening the "results" database,
	%%{ok, [Db]}. %%Store the connection to Db in State variable
    
	{ok, [S]}. %%Store the connection to CouchDb Server in State variable

	
handle_call({wordscore, Word}, From, State) ->
    spawn(fun() -> get_wordscore(Word, From, State) end),
   {noreply, State};

handle_call({storeres, List}, From, State) ->
    spawn(fun() -> store_result(List, From, State) end),
   {noreply, State};
   
handle_call({getres, Word}, From, State) ->
    spawn(fun() -> get_result(Word,From,State) end),
	{noreply, State}.
	
handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

%% Function to create the right object as an argument for word evaluation
make_Options(Word) ->
  SearchWord = list_to_binary(Word),
  [{key, SearchWord}].
  
get_wordscore(Word, From, State) ->
    DesignName = "posneg", %The name for the DesignDocument in wordlist-database specifying the view
    ViewName = "words_key_val", % The actual view
	Options = [],
	[S] = State, %%Use the connection to Server info which is stored in State
	{ok, Db}=couchbeam:open_db(S, "wordlist", Options), % opening the "wordlist" database,
	Options2 = make_Options(Word), %% Make the query key into right format
    {ok,ViewResults} = couchbeam_view:fetch(Db, {DesignName, ViewName},Options2), % returns rows corresponding to Word sent to function
	
	gen_server:reply(From, ViewResults).
	





%%Example of message store_result/3 function will receive:
%%[<<"2015">>,<<"12">>,<<"16">>,<<"soccer">>,<<"60">>,<<"13">>,<<"27">>]
store_result([Year,Month,Day,Neu,Neg,Pos,Subject],From,State) ->
    %%[Db] = State, %%Use the connection to Db info which is stored in State
    Options = [],
	[S]=State, %%Use the connection to Server info which is stored in State
	{ok, Db}=couchbeam:open_db(S, "results", Options), % opening the "results" database,
	
	Reply = "Result stored in Db",
	io:format("Subject searched for that is stored: ~ts~n", [Subject]),
    	
		Doc = {[{<<"Subject">>, Subject},
                {<<"Neutral Score">>, Neu},
                {<<"Negative Score">>, Neg},
                {<<"Positive Score">>, Pos},
				{<<"Year">>, Year},
				{<<"Month">>, Month},
				{<<"Day">>, Day}
                ]},
	couchbeam:save_doc(Db, Doc),
	gen_server:reply(From, Reply);
	
store_result([_],From,State) ->
	Reply = "Request to store in database is in invalid format",
	gen_server:reply(From, Reply).
	

get_result(Word,From,State)->
    
	DesignName = "getstats", %The name for the DesignDocument in reulst-database specifying the design doc
    ViewName = "stats", % The actual view
	%%[Db] = State, %%Use the connection to Db info which is stored in State
	Options = [],
	
	[S]=State, %%Use the connection to Server info which is stored in State
	{ok, Db}=couchbeam:open_db(S, "results", Options), % opening the "results" database,
    
	{ok,ViewResults} = couchbeam_view:fetch(Db, {DesignName, ViewName},[{key, Word}]), % returns rows corresponding to Word sent to function
    gen_server:reply(From, ViewResults).
