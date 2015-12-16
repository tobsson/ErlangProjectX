-module(projectx_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1, init/0, get_tweets/2]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    projectx_sup:start_link().

stop(_State) ->
    ok.
% Start the application
% erl -pa deps/*/ebin -pa ebin -s crypto -s ssl
% projectx_app:init()
init() ->
  ssl:start(),
  %register(tweet, spawn_link(fun loop/0)),
  io:format("Initialized projectx_app: ~n").

% This function will take the value of 'Query' and perform a search on
% twitter, returns a list with sentiment and 3 random tweets.
get_tweets(Query, Location) ->
  % Start ibrowse which we use for our network connections.
  ibrowse:start(),
  % Get Bearer token from twitter which we need for Application Auth
  BearerToken = app_auth(),
  %io:format("get_ tweets: BearerToken: ~p~n", [BearerToken]),
  % Construct the HTTP request with authentication and search parameters
  HeaderAuth = [{"Authorization","Bearer " ++ BearerToken}],
  % Create the URL for Twitter requests.
  URL = url_creator(Query, Location),

  % Request sent to Twitter
  {ok,_,_,TweetData} = ibrowse:send_req(URL, HeaderAuth, get),
  %io:format("get_tweets Returns: ~p~n", [TweetData]),

  % Run data through Jiffy to make it easier to handle in Erlang
  Jiffied = jiffy_decode(TweetData),
  io:format("Jiffied done ~n"),
  % Extract all the tweets text.
  AllText = extract_only_text(Jiffied, []),
  io:format("AllText done ~n"),
  % Analyze the texts and get a sentiment value back.
  Sentiment = word_server:textlist_val(AllText),
  io:format("sentiment set: ~p~n", [Sentiment]),
  spawn(fun () -> store(Query, Sentiment) end),
  % Extract 3 random tweets to display. IE in our Android app.
  Rando = random_tweets(Jiffied, [], 10),
  io:format("rando value set: ~p~n", [Rando]),
  % Return Sentiment and Random tweets in a list.
  Sentiment ++ Rando.

% This function creates the URL to query Twitter
% based on what parameters should be used.
url_creator(Query, undefined) ->
  URIQuery  = http_uri:encode(Query),
  string:concat(string:concat(
          "https://api.twitter.com/1.1/search/tweets.json?q=",URIQuery),
            "&count=15&lang=en&result_type=recent");
url_creator(Query, Location) ->
  URIQuery  = http_uri:encode(Query),
  io:format("url_creator Location: ~p~n", [Location]),
  string:concat(string:concat(string:concat(
          "https://api.twitter.com/1.1/search/tweets.json?q=",URIQuery),
            "&count=15&lang=en&result_type=recent&geocode="), Location).

% Takes a JSON object and makes it more readable.
jiffy_decode(A) ->
  TweetDataDecoded = jiffy:decode(A),
  {TDD} = TweetDataDecoded, % extracts list from first tuple
  % Extracts the only tuple "statuses" from list
  {_Key, Value} = lists:keyfind(<<"statuses">>, 1, TDD),
  %io:format("jiffy_decode: ~p~n", [Value]),
  Value.

% Chooses 3 random tweets
	random_tweets(Value, Data, 7) -> Data;
	random_tweets(Value, Data, N) ->
		{RandomTweet} = lists:nth(random:uniform(N), Value),% random:uniform(N) chooses random number from range 1-N
		Value1 = lists:delete({RandomTweet}, Value),
		{KeyUser, ValueUser} = lists:keyfind(<<"user">>, 1, RandomTweet),
		{VUser} = ValueUser,
		{KeyName, UserName} = lists:keyfind(<<"name">>, 1, VUser),
		{RKey, RandomText} = lists:keyfind(<<"text">>, 1, RandomTweet),
		random_tweets(Value, Data ++ [UserName] ++ [RandomText], N-1).
%Loops it (loop should be executed only 3 times) and put usernames and tweets in a loop


% Extracts only the fields with "text" from JSON.
% List and Data is a Binary List
extract_only_text([], Data) -> Data;
extract_only_text(List, Data) ->
  % Extracts first tuple from the list Value
  {Head} = hd(List),
  {_Key, Text} = lists:keyfind(<<"text">>, 1, Head),
  extract_only_text(tl(List), Data ++ [Text]).

store(Query, [_,Neu,_,Neg,_,Pos]) ->
  {Year,Month,Day} = date(),
  Atoms   = [Year,Month,Day],
  Strings = [integer_to_list(X) || X <- Atoms] ++ [Query],
  Binary  = [list_to_binary(Y) || Y <- Strings],
  List = Binary ++ [Neu] ++ [Neg] ++ [Pos],
  io:format("List for Storing: ~p~n",[List]).


% This function returns a Bearer Token from Twitter
% that's needed for Application Authentication
app_auth() ->
  % ConsumerKey and Secret are specific to this an app and can be found here:
  % https://apps.twitter.com/
  % This function will encode the values, construct the HTTP request and
  % return a Bearer Token as a list
  ConsumerKey     = "JNEVsG01mMdYIKq404MMmXw2H",
  ConsumerSecret  = "aeNjhFV0F02PjNktQEBN2ESGbbzbDlcNeVufBxsKI9B0qFjFhB",
  EncodedConsumerKey    = http_uri:encode(ConsumerKey),
  EncodedConsumerSecret = http_uri:encode(ConsumerSecret),
  ReadyToBase = string:concat(
    string:concat(EncodedConsumerKey, ":"), EncodedConsumerSecret),
  Based   = base64:encode_to_string(ReadyToBase),
  Body    = "grant_type=client_credentials",
  Headers = [{"Authorization","Basic " ++ Based},
              {"Content-Type","application/x-www-form-urlencoded;charset=UTF-8"}],
  % Request the Bearer Token
  {ok,_,_,TokenAnswer} = ibrowse:send_req(
                            "https://api.twitter.com/oauth2/token",
                                Headers, post, Body),

  % Turn the answer into something more readable
  {TokenDecoded}    = jiffy:decode(TokenAnswer),
  % Check that it really is a bearer token
  %{_,TokenType}     = lists:keyfind(<<"token_type">>, 1, TokenDecoded),
  % Extract the Token and put it into a list.
  {_,BearerToken}   = lists:keyfind(<<"access_token">>, 1, TokenDecoded),
  BearerTokenString = binary:bin_to_list(BearerToken),
  BearerTokenString.
