-module(word_server).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% NOTE: This module needs Couchbeam to be available to work with CouchDb database

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, word_val/1,text_val/1, textlist_val/1]).

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

%Function to evaluate 1 word
word_val(Word) ->
    gen_server:call(?SERVER,{wordval, Word}, infinity).

% Function to evaluate a whole text
text_val(Text) ->
    gen_server:call(?SERVER,{textval, Text}, infinity).

% Function to evaluate a whole list of several texts
textlist_val(List) ->
	gen_server:call(?SERVER,{listval, List}, infinity).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

%Starts couchbeam upon gen_server startup and establishes a connection to our database for wordlist
init([]) ->
    couchbeam:start(),
	Url = "localhost:5984",
    Options = [],
    S = couchbeam:server_connection(Url, Options),  % connect to the server
    {ok, Db}=couchbeam:open_db(S, "wordlist", Options), % opening the "wordlist" database,
	{ok, [Db]}. %%Store the connection to Db in State variable


%Call from word_val/1 function
%Replies with pos, neg, neutral score for one word
handle_call({wordval, Word}, _From, State) ->
    Points = word_Eval(Word, State),
	{reply, Points, State};

% Call from text_val/1
% Replies with a score for a text, pos, neg or neutral
handle_call({textval,Text}, From, State) ->
  %%io:format("The from of it all:~p~n", [From]),
   spawn(fun() -> text_Eval(Text, From, State) end),
   {noreply, State};

% Call from textlist_val/1
% Replies with a score for a list texts with a list of points
handle_call({listval,List}, From, State) ->
  %%io:format("textlist_val called from:~p~n", [From]),
   spawn(fun() -> textlist_Eval(List, From, State) end),
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


% Function for making the query options usable with couchbeams query to CouchDB view
% The "options" for searching for rows in a view containing ONE specific key
% takes the form [{key, OPTIONS}] where OPTIONS is a binary() of the key, that is a word

 make_Options(Word) ->
  SearchWord = list_to_binary(Word),
  [{key, SearchWord}].


% Function evaluates a word against the "wordlist" document's "words_key_val"-view in CouchDB
% It makes a connection and evaluates the Word which is sent to the function and return -1, 1 or 0.

  word_Eval(Word, State) ->
    DesignName = "posneg", %The name for the DesignDocument in wordlist-database specifying the view
    ViewName = "words_key_val", % The actual view
	[Db] = State, %%Use the connection to Db info which is stored in State
	Options2 = make_Options(Word), %% Make the query key into right format
    {ok,ViewResults} = couchbeam_view:fetch(Db, {DesignName, ViewName},Options2), % returns rows corresponding to Word sent to function

	% Case statement to evaluate if we get a key and value form DB or an empty list(when the word isn't in our wordlist)
	% giving a score from the value if a key is found in db otherwise it returns "0".

	  case ViewResults of
	    [_] ->
	      {Value}=hd(ViewResults),
	      ValueTuple = lists:keyfind(<<"value">>, 1, Value),
	      {_, X} = ValueTuple,
	      X;
	    []-> 0

	  end.


% Function to evaluate a whole text, splits the text's words into a list separating by spaces
% And make use of eval_word/1 for each word
% Function returns a total score for the received Text calculated by the sum of scores for each word in text

%first function used internally with textlist_Eval/3
  text_Eval(BinText, State) ->
    Self = self(),
    %%io:format("BinText from text_Eval/2: ~ts~n", [BinText]),
    Text = binary:bin_to_list(BinText),
    Tokens = string:tokens(Text, " "), % split by spaces into list
    %%PointsList=[word_Eval(N, State) || N <- Tokens], %Create a list with all scores for each word
	Pids = [spawn_link(fun () ->Self ! {self(), word_Eval(N, State)} end) || N <- Tokens],
    PointsList = [receive {Pid, R} -> R end || Pid <- Pids ],
	
	
    Total = sum(PointsList),      % summing the list
    %%io:format("The list of points:~p~n", [PointsList]), % JUST TEST to see what the scores for each words are
      if
       Total == 0 -> 0;
       Total <  0 -> -1;
       Total >  0 -> 1

      end.

% Function which runs when you use text_val/1 
  text_Eval(BinText, From, State) ->
    Self = self(),
    %%io:format("BinText from text_Eval/3 ~ts~n", [BinText]),
    Text = binary:bin_to_list(BinText),
    Tokens = string:tokens(Text, " "), % split by spaces into list
    %%PointsList=[word_Eval(N, State) || N <- Tokens], %Create a list with all scores for each word
	Pids = [spawn_link(fun () ->Self ! {self(), word_Eval(N, State)} end) || N <- Tokens],
    PointsList = [receive {Pid, R} -> R end || Pid <- Pids ],
	
	
    Total = sum(PointsList),      % summing the list
    %%io:format("The list of points:~p~n", [PointsList]), % JUST TEST to see what the scores for each words are
      if
       Total == 0 -> gen_server:reply(From, 0);
       Total <  0 -> gen_server:reply(From, -1);
       Total >  0 -> gen_server:reply(From, 1)

      end.

%% Function for checking list of texts
  textlist_Eval(List, From, State) ->

	%Create a list with all scores for each text
    Self = self(),
	Pids = [spawn_link(fun () ->Self ! {self(), text_Eval(N, State)} end) || N <- List],
    TextList = [receive {Pid, R} -> R end || Pid <- Pids ],
    list_Eval(TextList, From).

%% Function for calculating percentage of all text's values from a list
%% Doesnt alway land on 100% !!! YET!!
  list_Eval(List, From) ->
	
	Total = (100/length(List)),
	NeutralList=lists:filter(fun(X) -> X == 0 end, List),
	NegativeList=lists:filter(fun(X) -> X < 0 end, List),
	PositiveList=lists:filter(fun(X) -> X > 0 end, List),
	NeutralReplies = float_to_list((length(NeutralList)* Total),[{decimals,0}]),
	NegativeReplies = float_to_list((length(NegativeList)* Total),[{decimals,0}]),
	PositiveReplies = float_to_list((length(PositiveList)* Total),[{decimals,0}]),
	Result = ["Neutral:", NeutralReplies,
            "Negative:", NegativeReplies,
            "Positive:", PositiveReplies],
  BinaryResult = [erlang:list_to_binary(A) || A <- Result],

	gen_server:reply(From, BinaryResult).


% Simple sum-function to sum the list

  sum(L) ->
     sum(L, 0).

  sum([H|T], Acc) ->
     sum(T, H + Acc);

  sum([], Acc) ->
     Acc.
